import os
import json
import requests
from bs4 import BeautifulSoup
from urllib.parse import urlparse

# Configuration
INPUT_FILE = "links.txt"
OUTPUT_FOLDER = "downloaded_images"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
}

def download_images():
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)

    with open(INPUT_FILE, "r") as f:
        links = list(set([line.strip() for line in f if line.strip()])) # Remove duplicates

    for url in links:
        try:
            print(f"Processing: {url}")
            response = requests.get(url, headers=HEADERS, timeout=15)
            response.raise_for_status()
            
            soup = BeautifulSoup(response.text, 'html.parser')
            
            # 1. Try to find the data in the Next.js JSON block
            script_tag = soup.find('script', id='__NEXT_DATA__')
            img_url = None

            if script_tag:
                data = json.loads(script_tag.string)
                # This path is common for Christie's lot pages
                try:
                    # Search deeply for the primary image
                    # The structure can be complex, so we look for 'image' keys
                    lot_data = data['props']['pageProps']['initialState']['lot']['details']
                    img_url = lot_data.get('image', {}).get('src')
                except (KeyError, TypeError):
                    pass

            # 2. Fallback: If JSON path failed, try finding the URL via text search
            if not img_url:
                import re
                # Look for the specific Christie's image pattern in the raw HTML text
                pattern = r'https://www\.christies\.com/img/LotImages/[^"\'>]+\.jpg'
                match = re.search(pattern, response.text)
                if match:
                    img_url = match.group(0)

            if img_url:
                # Clean the URL as you requested (remove hash and width)
                clean_url = img_url.split('&')[0]
                
                # Get default filename
                parsed = urlparse(clean_url)
                filename = os.path.basename(parsed.path)
                
                # Download
                print(f"Downloading: {filename}")
                img_response = requests.get(clean_url, headers=HEADERS)
                with open(os.path.join(OUTPUT_FOLDER, filename), 'wb') as f:
                    f.write(img_response.content)
                print("Success!")
            else:
                print("Failed: Image URL not found in page source.")

        except Exception as e:
            print(f"Error on {url}: {e}")

if __name__ == "__main__":
    download_images()