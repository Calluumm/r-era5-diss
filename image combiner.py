from PIL import Image, ImageDraw, ImageFont
import cv2
import numpy as np
import os

ph = "File//path"

#4 panel combine

image1 = cv2.imread(os.path.join(ph, 'im 1.png')) #temp top right is fitting
image2 = cv2.imread(os.path.join(ph, 'im 2.png')) # U component slot
image3 = cv2.imread(os.path.join(ph, 'im 3.png'))
image4 = cv2.imread(os.path.join(ph, 'im 4.png')) # V component slot
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


filename = 'name.jpg'
cv2.imwrite(os.path.join(ph, filename), combined_image)


#top bottom combine

image1 = cv2.imread(os.path.join(ph, 'top_image.png')) # Winter
image2 = cv2.imread(os.path.join(ph, 'bottom_image.png')) # Summer

height, width = image1.shape[:2]
image2 = cv2.resize(image2, (width, height))


combined_image = np.vstack((image1, image2))


filename = 'Figure 3-4 FINAL.jpg'
cv2.imwrite(os.path.join(ph, filename), combined_image)

print("Combined image saved as", filename)



