"""Download Magellan–Elcano route lines from the public Esri FeatureServer.

The script uses only Python's standard library. It writes each ArcGIS layer as
GeoJSON and also creates one combined file for convenient mapping in R or
Python. Coordinates are requested in WGS84 (EPSG:4326).
"""

from __future__ import annotations

import json
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any
from urllib.parse import urlencode
from urllib.request import Request, urlopen


SERVICE_URL = (
    "https://services.arcgis.com/6DIQcwlPy8knb6sg/arcgis/rest/services/"
    "Magellan/FeatureServer"
)

ROUTE_LAYERS = {
    0: "magellan2",
    1: "magellan",
    2: "del_cano",
    3: "alternative_return",
}

OUTPUT_DIR = Path(__file__).resolve().parent / "data"


def request_json(url: str, parameters: dict[str, Any] | None = None) -> dict[str, Any]:
    if parameters:
        url = f"{url}?{urlencode(parameters)}"

    request = Request(url, headers={"User-Agent": "magellan-route-downloader/1.0"})
    with urlopen(request, timeout=60) as response:
        payload = json.load(response)

    if "error" in payload:
        message = payload["error"].get("message", "Unknown ArcGIS REST error")
        details = "; ".join(payload["error"].get("details", []))
        raise RuntimeError(f"{message}: {details}".rstrip(": "))

    return payload


def epoch_milliseconds_to_iso(value: Any) -> str | None:
    if not isinstance(value, (int, float)):
        return None

    epoch = datetime(1970, 1, 1, tzinfo=timezone.utc)
    return (epoch + timedelta(milliseconds=value)).date().isoformat()


def polyline_to_geojson(geometry: dict[str, Any]) -> dict[str, Any] | None:
    paths = geometry.get("paths", [])
    if not paths:
        return None

    if len(paths) == 1:
        return {"type": "LineString", "coordinates": paths[0]}

    return {"type": "MultiLineString", "coordinates": paths}


def download_layer(layer_id: int, file_stem: str) -> dict[str, Any]:
    layer_url = f"{SERVICE_URL}/{layer_id}"
    metadata = request_json(layer_url, {"f": "json"})

    query = request_json(
        f"{layer_url}/query",
        {
            "where": "1=1",
            "outFields": "*",
            "returnGeometry": "true",
            "outSR": 4326,
            "orderByFields": metadata["objectIdField"],
            "f": "json",
        },
    )

    date_fields = {
        field["name"]
        for field in metadata.get("fields", [])
        if field.get("type") == "esriFieldTypeDate"
    }

    features = []
    for source_feature in query.get("features", []):
        geometry = polyline_to_geojson(source_feature.get("geometry", {}))
        if geometry is None:
            continue

        properties = dict(source_feature.get("attributes", {}))
        for field_name in date_fields:
            properties[f"{field_name}_iso"] = epoch_milliseconds_to_iso(
                properties.get(field_name)
            )

        properties["source_layer_id"] = layer_id
        properties["source_layer"] = metadata["name"]

        features.append(
            {
                "type": "Feature",
                "properties": properties,
                "geometry": geometry,
            }
        )

    collection = {
        "type": "FeatureCollection",
        "name": metadata["name"],
        "source": layer_url,
        "features": features,
    }

    output_file = OUTPUT_DIR / f"{file_stem}.geojson"
    output_file.write_text(
        json.dumps(collection, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )

    print(f"{metadata['name']}: {len(features)} features -> {output_file}")
    return collection


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    combined_features = []
    for layer_id, file_stem in ROUTE_LAYERS.items():
        collection = download_layer(layer_id, file_stem)
        combined_features.extend(collection["features"])

    combined = {
        "type": "FeatureCollection",
        "name": "Magellan–Elcano routes",
        "source": SERVICE_URL,
        "downloaded_utc": datetime.now(timezone.utc).isoformat(),
        "features": combined_features,
    }

    combined_file = OUTPUT_DIR / "magellan_elcano_routes.geojson"
    combined_file.write_text(
        json.dumps(combined, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )

    print(f"Combined: {len(combined_features)} features -> {combined_file}")


if __name__ == "__main__":
    main()
