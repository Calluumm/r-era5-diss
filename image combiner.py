import cv2
import numpy as np
import os

ph = "C:\\Users\\Student\\Desktop\\Dissertation\\Data related"

image1 = cv2.imread(os.path.join(ph, 'SUMMERTEMPGRAPH.png')) #temp top right is fitting
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
cv2.line(combined_image_ba, (2*width, height//2), (2*width + annotation_width//4, height//4), line_color, line_thickness)
cv2.line(combined_image_ba, (2*width, 3*height//2), (2*width + annotation_width//4, 1*height//4), line_color, line_thickness)
font = cv2.FONT_HERSHEY_SIMPLEX
font_scale = 2.0
font_color = (0, 0, 0)
font_thickness = 10
font_scale2 = 1.5
font_thickness2 = 5
cv2.putText(combined_image_ba, 'U + V components', (2*width + annotation_width//4 + 10, height//2 - 500), font, font_scale, font_color, font_thickness)
multi_line_text = [
    "Wind can be split into 2 components",
    "u represents the south to north",
    "movement of wind whereas v accounts for",
    "the west to east movement.",
    "They can be combined into a single windspeed",
    "component to see an overall magnitude",
    "difference but when left in their individual",
    "components you can see the change in wind",
    "direction for an area. In this",
    "case there is a positive increase in",
    "u-component so an increase in winds from",
    "the south, and a negative v-component",
    "so an increase of winds from the east."
]
y0 = height//4 + 50
dy = 40 
for i, line in enumerate(multi_line_text):
    y = y0 + i * dy
    cv2.putText(combined_image_ba, line, (2*width + annotation_width//4 + 10, y), font, font_scale2, font_color, font_thickness2)


filename = 'summer_combined.jpg'
cv2.imwrite(os.path.join(ph, filename), combined_image_ba)

print("Combined image saved as", filename)
