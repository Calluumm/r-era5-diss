from PIL import Image, ImageDraw, ImageFont
import cv2
import numpy as np
import os

ph = "C:\\file_path"

image1 = cv2.imread(os.path.join(ph, 'whateverimage.png/jpg')) #temp top right is fitting
image2 = cv2.imread(os.path.join(ph, 'SUMMERUGRAPH.png')) # U component slot
image3 = cv2.imread(os.path.join(ph, 'SUMMERPGRAPH.png'))
image4 = cv2.imread(os.path.join(ph, 'SUMMERVGRAPH.png')) # V component slot
if image1 is None or image2 is None or image3 is None or image4 is None:
    print("Error: One or more images not found.")
    exit()

height, width = image1.shape[:2]
image2 = cv2.resize(image2, (width, height))
image3 = cv2.resize(image3, (width, height))
image4 = cv2.resize(image4, (width, height))

top_row = np.hstack((image1, image2))
bottom_row = np.hstack((image3, image4))
combined_image = np.vstack((top_row, bottom_row))

# box the joint components, both on right side of image so annotations can go on the right nicely.
box_start = (width, 0)
box_end = (2*width, 2*height)
cv2.rectangle(combined_image, box_start, box_end, (255, 30, 0), 20) 

border_size = 20
border_color = (255, 255, 255)  # White color
combined_image_b = cv2.copyMakeBorder(combined_image, border_size, border_size, border_size, border_size, cv2.BORDER_CONSTANT, value=border_color)

annotation_width = 1200  
combined_image_ba = cv2.copyMakeBorder(combined_image_b, 0, 0, 0, annotation_width, cv2.BORDER_CONSTANT, value=(255, 255, 255))
line_color = (255, 30, 0)  
line_thickness = 15
#cv2 line is essentially just (file, start, end, colour, thickness)
cv2.line(combined_image_ba, (2*width, height//2), (2*width + annotation_width//4, height//4), line_color, line_thickness)
cv2.line(combined_image_ba, (2*width, 3*height//2), (2*width + annotation_width//4, 1*height//4), line_color, line_thickness)
#make it PIL so not constrained to cv2 fonts
combined_image_ba_pil = Image.fromarray(cv2.cvtColor(combined_image_ba, cv2.COLOR_BGR2RGB))
draw = ImageDraw.Draw(combined_image_ba_pil)

font_path = "arial.ttf" #or any old font
font = ImageFont.truetype(font_path, 80)
font2 = ImageFont.truetype(font_path, 60)

draw.text((2*width + annotation_width//4 - 50, height//2 - 700), 'U + V trends', font=font, fill=(0, 0, 0))
multi_line_text = [
    'Increased windspeeds from',
    'the X and X.',
    'It\'s not clear if these',
    'directional increases are',
    'independant of one another.'
]
y0 = height//2 - 625
dy = 60 
for i, line in enumerate(multi_line_text):
    y = y0 + i * dy
    draw.text((2*width + annotation_width//4 - 50, y), line, font=font2, fill=(0, 0, 0))

draw.text((2*width + annotation_width//4 - 50, height//2 + 600), 'Correlation Coefficients', font=font, fill=(0, 0, 0))
multi_line_text2 = [
    'Temperature: coeff',
    'U Component: coeff',
    'V Component: coeff',
    'Precip. Spearmans: coeff'
]
y01 = height//2 + 675
dy = 60 
for i, line in enumerate(multi_line_text2):
    y1 = y01 + i * dy
    draw.text((2*width + annotation_width//4 - 50, y1), line, font=font2, fill=(0, 0, 0))

# back to cv2
combined_image_ba = cv2.cvtColor(np.array(combined_image_ba_pil), cv2.COLOR_RGB2BGR)

filename = 'summer_combined_f.jpg'
cv2.imwrite(os.path.join(ph, filename), combined_image_ba)

print("Combined image saved as", filename)
filename = 'NAME.jpg or png'
cv2.imwrite(os.path.join(ph, filename), combined_image_ba)

print("Combined image saved as", filename)
