#include "tracker/ChessboardCameraTrackerKLT.hpp"

#include "tracker/utility.hpp"
#include <iostream>

#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/video/tracking.hpp>

using namespace std;
using namespace cv;

/**
 * It detects a chessboard inside an image and if found it returns the pose of the camera wrt the chessboard
 *
 * @param[in,out] view the original image
 * @param[out] pose the pose of the camera
 * @param[in] cam the camera
 * @param[in] boardSize the size of the chessboard to detect
 * @param[in] pattern the type of pattern to detect
 * @return true if the chessboard has been found
 */
bool ChessboardCameraTrackerKLT::process(
  cv::Mat& view, cv::Mat& pose, const Camera& cam, const cv::Size& boardSize, const Pattern& pattern)
{
    // true if the chessboard is found
    bool found = false;

    Mat temp = view.clone(); // used for correcting the optical distortion

    //******************************************************************/
    // undistort the input image. view at the end must contain the undistorted version
    // of the image.
    //******************************************************************/


    // contains the grey version of the current frame
    Mat viewGrey;

    //******************************************************************/
    // convert the current left frame into greylevel image
    //******************************************************************/


    // if we have too few points or none
    if(_corners.size() < 10)
    {
        //******************************************************************/
        // detect the chessboard
        //******************************************************************/


        if(found)
        {
            //******************************************************************/
            // generate the points on the chessboard, this time 3D points
            // see --> calcChessboardCorners3D
            //******************************************************************/


            //******************************************************************/
            // compute the pose of the camera using mySolvePnPRansac (utility.hpp))
            //******************************************************************/

        }
    }
    else
    {
        // use klt to track the points

        // some parameters for the optical flow algorithm
        Size winSize(11, 11);
        TermCriteria termcrit(CV_TERMCRIT_ITER | CV_TERMCRIT_EPS, 20, 0.03);

        // vector where the estimated tracked points of the new frame will be stored
        vector<Point2f> currPts;

        // auxiliary stuff for the optical flow computation
        // status has will have the same length as currPts and is 0 if the
        // optical flow estimation for the corresponding new point is not good
        vector<uchar> status;
        vector<float> err;

        //******************************************************************/
        // estimate the new position of the tracked points using calcOpticalFlowPyrLK
        //******************************************************************/


        //******************************************************************/
        // Filter currPts and update the lists _corners and _objectPoints: if
        // the corresponding value in status is >0  then copy the new point of
        // currPts in _corner, otherwise skip the point.
        //******************************************************************/

        // i is used to run through all the elements of currPts
        // k is used to run through _corners and _objectPoints to keep only the well tracked features
        size_t i, k;

        for(i = k = 0; i < currPts.size(); i++)
        {
            //******************************************************************/
            // if it's a good point copy it in _corners and also copy keep the
            // corresponding _objectPoints
            //******************************************************************/
            // if..
            {
#if DEBUGGING
                line(view, _corners[i], currPts[i], Scalar(255, 0, 0), 1);
                circle(view, currPts[i], 3, Scalar(255, 0, 255), -1, 8);
#endif
                //******************************************************************/
                // copy the current point in _corners
                //******************************************************************/


                //******************************************************************/
                // copy the corresponding _objectPoints
                //******************************************************************/


                //******************************************************************/
                // update k
                //******************************************************************/
                ++k;
            }
        }

        // resize the two vector to the size k, the number of "well" tracked features
        _corners.resize(k);
        _objectPoints.resize(k);

        // vector containing the inliers
        vector<int> idxInl;

        //******************************************************************/
        // compute the pose of the camera using mySolvePnPRansac (utility.hpp))
        //******************************************************************/

        //        _vecCorners.push_back(_corners);
        ////        _vecObjectPoints.push_back(_objectPoints);
        //        _vecIdx.push_back(_indices);

        //******************************************************************/
        // filter the points to remove the outliers. Use filterVector from utility.hpp
        // Filter both the image points and the 3D reference points
        //******************************************************************/



        found = true;
    }

    //******************************************************************/
    // update _prevGrey with the current grey frame
    //******************************************************************/


    return found;
}
