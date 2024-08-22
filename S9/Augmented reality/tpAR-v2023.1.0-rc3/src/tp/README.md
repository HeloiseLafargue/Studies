## Detecting the chessboard

In this first exercise we want to build a program that given an image as input, detect the a chessboard in the image and draw the detected chessboard corners 

```bash
./bin/checkerboard -w 8 -h 6 ../data/images/left01re.jpg
```

## Detecting the chessboard on a video

Now that we can detect a chessboard on an image, let’s move one step forward and write a program that detected the chessboard in a given video stream.

```bash
./bin/checkerboardVideo -w 9 -h 6 ../data/video/calib.avi
```

### Perspective rectification

In this exercise we will modify the previous code in order to estimate the homography in a robust way every time the chessboard is detected.

```bash
./bin/checkerboardvideoRectified -w 9 -h 6 ../data/video/calib.avi
```

## Camera calibration

In order to calibrate the camera we will rely on a program that comes with OpenCV.

```bash
./bin/calibration -w 9 -h 6 -V -n 10 -zt -o calib.xml ../data/video/calib.avi
```

### Removing the optical distortion

We want to implement a program that given a video and the relevant camera parameters as input, will remove the optical distortion and show the undistorted image.

```bash
./bin/checkerboardvideoUndistort -c calib.xml ../data/video/calib.avi
```

## The camera tracker

This first version of the camera tracker implements a tracking by detection method: each frame is processed independently to detect the chessboard and eventually compute the camera pose.

```bash
./bin/tracking -w 9 -h 6 -c calib.xml ../data/video/calib.avi
```

## The KLT version

In this exercise we will build a camera tracker that detect the chessboard and then track the corners using the Kanade-Lucas- Tomasi method (KLT).

```bash
./bin/trackingKLT -w 9 -h 6 -c calib.xml ../data/video/calib.avi
```

## Adding the OpenGL rendering

We will use OpenGL to render the 3D object on top of the chessboard.

```bash
./bin/videoOGLTeapotTP -w 9 -h 6 -c calib.xml ../data/video/calib.avi
```

## Rendering an OBJ 3D object

The last step of this TP is to replace the usual OpenGL teapot with a generic 3D object loaded from an Wavefront OBJ file.

```bash
./bin/videoOGLTracking -w 9 -h 6 -c calib.xml -o ../data/models/superma.obj ../data/video/calib.avi
```