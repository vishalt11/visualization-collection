from rembg import remove
from PIL import Image
import io

input_path = 'img2.jpg'
output_path = 'img2_nb.png'

# Open the image
with open(input_path, 'rb') as i:
    input_image = i.read()
    # Remove the background
    output_image = remove(input_image)
    
    # Save the result
    with open(output_path, 'wb') as o:
        o.write(output_image)

print("background removed!")