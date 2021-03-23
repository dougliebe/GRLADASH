import numpy as np
import argparse
import cv2
import imutils
import time
blueLower = (150, 0, 0)
blueUpper = (255, 150, 150)

# Load an color image in grayscale
img = cv2.imread('images/prac_image.png')
# img = img[181:1092,179:667]
img = img[179:667, 181:1092]
# resize the frame, blur it, and convert it to the HSV
# color space
# frame = imutils.resize(frame, width=600)
blurred = cv2.GaussianBlur(img, (21, 21), 0)
# hsv = cv2.cvtColor(blurred, cv2.COLOR_BGR2HSV)
# construct a mask for the color "green", then perform
# a series of dilations and erosions to remove any small
# blobs left in the mask
mask = cv2.inRange(blurred, blueLower, blueUpper)
mask = cv2.erode(mask, None, iterations=2)
mask = cv2.dilate(mask, None, iterations=2)

# find contours in the mask and initialize the current
# (x, y) center of the ball
cnts = cv2.findContours(mask.copy(), cv2.RETR_EXTERNAL,
	cv2.CHAIN_APPROX_SIMPLE)
cnts = imutils.grab_contours(cnts)
center = None
# only proceed if at least one contour was found
if len(cnts) > 0:
	# find the largest contour in the mask, then use
	# it to compute the minimum enclosing circle and
	# centroid
	for i in cnts:
		c = i
		((x, y), radius) = cv2.minEnclosingCircle(c)
		M = cv2.moments(c)
		center = (int(M["m10"] / M["m00"]), int(M["m01"] / M["m00"]))
		# only proceed if the radius meets a minimum size
		if radius > 5:
			# draw the circle and centroid on the frame,
			# then update the list of tracked points
			# cv2.circle(img, (int(x), int(y)), int(radius),
			# 	(0, 255, 255), 2)
			cv2.circle(img, center, 5, (0, 0, 255), -1)


cv2.imshow('image',mask)
cv2.imshow('image2',img)
while(1):
	key = cv2.waitKey(1) & 0xFF
	# if the 'q' key is pressed, stop the loop
	if key == ord("q"):
		break

cv2.destroyAllWindows()
