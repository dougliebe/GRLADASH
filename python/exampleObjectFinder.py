# import the necessary packages
from track.centroidtracker import CentroidTracker
from track.centroidfinder import CentroidFinder
from imutils.video import VideoStream
from pytube import YouTube
import numpy as np
import argparse
import imutils
import time
import cv2
# construct the argument parse and parse the arguments
ap = argparse.ArgumentParser()
#ap.add_argument("-p", "--prototxt", required=True,
#	help="path to Caffe 'deploy' prototxt file")
#ap.add_argument("-m", "--model", required=True,
#	help="path to Caffe pre-trained model")
ap.add_argument("-c", "--confidence", type=float, default=0.5,
	help="minimum probability to filter weak detections")
args = vars(ap.parse_args())

# initialize our centroid tracker and frame dimensions
ct = CentroidTracker()
cf = CentroidFinder()
(H, W) = (None, None)
## load our serialized model from disk
#print("[INFO] loading model...")
#net = cv2.dnn.readNetFromCaffe(args["prototxt"], args["model"])
# initialize the video stream and allow the camera sensor to warmup
print("[INFO] starting video stream...")
yt = YouTube("https://www.youtube.com/watch?v=RbTu-6lGk8E&feature=youtu.be&ab_channel=LAGuerrillasVOD")
stream = yt.streams.filter(file_extension="mp4", res="720p")[0]
cap = cv2.VideoCapture(stream.url)
cap.set(cv2.CAP_PROP_POS_FRAMES, 2*30)
t = time.time()
frame_count = 0
# loop over the frames from the video stream
while True:
	# read the next frame from the video stream and resize it
	ret, im = cap.read()
	frame = im[179:667, 181:1092]
	frame_count += 1
	# if the frame dimensions are None, grab them
	if W is None or H is None:
		(H, W) = frame.shape[:2]
#	# construct a blob from the frame, pass it through the network,
#	# obtain our output predictions, and initialize the list of
#	# bounding box rectangles
#	blob = cv2.dnn.blobFromImage(frame, 1.0, (W, H),
#		(104.0, 177.0, 123.0))
#	net.setInput(blob)
#	detections = net.forward()
	pts = []
	# run finder to get set of x, y centroids
	detections = cf.findInImage(frame)
#	print(detections)
	# loop over the detections
	for i in range(0, len(detections)):
		pts.append(detections[i])
#		# filter out weak detections by ensuring the predicted
#		# probability is greater than a minimum threshold
#		if detections[0, 0, i, 2] > args["confidence"]:
#			# compute the (x, y)-coordinates of the bounding box for
#			# the object, then update the bounding box rectangles list
#			box = detections[0, 0, i, 3:7] * np.array([W, H, W, H])
#			rects.append(box.astype("int"))
#			# draw a bounding box surrounding the object so we can
#			# visualize it
#			(startX, startY, endX, endY) = box.astype("int")
#			cv2.rectangle(frame, (startX, startY), (endX, endY),
#				(0, 255, 0), 2)
	# update our centroid tracker using the computed set of bounding
	# box rectangles
#	print(pts)
	objects = ct.update(pts)
	# loop over the tracked objects
	for (objectID, centroid) in objects.items():
		# draw both the ID of the object and the centroid of the
		# object on the output frame
		text = "ID {}".format(objectID)
		cv2.putText(frame, text, (centroid[0] - 10, centroid[1] - 10),
			cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 2)
		cv2.circle(frame, (centroid[0], centroid[1]), 4, (0, 255, 0), -1)
	# show the output frame
	cv2.imshow("Frame", frame)
	key = cv2.waitKey(1) & 0xFF
	# if the `q` key was pressed, break from the loop
	if key == ord("q"):
		break
# do a bit of cleanup
cv2.destroyAllWindows()
cap.release()
print(frame_count)
print((frame_count/(time.time()-t))/30)
print("done")
