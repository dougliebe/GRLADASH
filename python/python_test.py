from track.centroidfinder import CentroidFinder
import cv2

im = cv2.imread('images/overlap.png')[179:667, 181:1092]
blurred = cv2.GaussianBlur(im, (21,21),0)
mask = cv2.inRange(blurred, (150,0,0), (255,150,150))
mask = cv2.erode(mask, None, iterations = 3)
#mask = cv2.dilate(mask, None, iterations = 2)

cf = CentroidFinder()
c = cf.findInImage(im)
for i in c:
	cv2.circle(im, (int(i[0]), int(i[1])), int(5), (0,255,255), -1)

cv2.imshow("test", im)
cv2.imshow("mask", mask)
while(1):
	key = cv2.waitKey(1) & 0xFF
	if key == ord('q'):
		break
cv2.destroyAllWindows()
