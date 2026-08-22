"""Download Vasco da Gama's 1497–1498 voyage from a public Esri service.

The script uses only Python's standard library and writes the route as
GeoJSON in ``17_voyage/data``. Coordinates are requested in WGS84
(EPSG:4326), ready for use with sf, geopandas, or other GIS software.
"""

from __future__ import annotations

import json
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen


SERVICE_URL = (
    "https://services.arcgis.com/CCZiGSEQbAxxFVh3/ArcGIS/rest/services/"
    "Descobrimentos/FeatureServer"
)
LAYER_ID = 3
OUTPUT_DIR = Path(__file__).resolve().parent / "data"
OUTPUT_FILE = OUTPUT_DIR / "vasco_da_gama_1497_1498.geojson"


def request_json(url: str, parameters: dict[str, Any] | None = None) -> dict[str, Any]:
    if parameters:
        url = f"{url}?{urlencode(parameters)}"

    request = Request(url, headers={"User-Agent": "vasco-da-gama-route-downloader/1.0"})
    for attempt in range(1, 4):
        try:
            with urlopen(request, timeout=60) as response:
                payload = json.load(response)
            break
        except (HTTPError, URLError, TimeoutError):
            if attempt == 3:
                raise
            time.sleep(2**attempt)

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


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    layer_url = f"{SERVICE_URL}/{LAYER_ID}"
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

        properties["voyage"] = "Vasco da Gama, 1497–1498"
        properties["source_layer_id"] = LAYER_ID
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
        "downloaded_utc": datetime.now(timezone.utc).isoformat(),
        "features": features,
    }

    OUTPUT_FILE.write_text(
        json.dumps(collection, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )

    print(f"{metadata['name']}: {len(features)} features -> {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
