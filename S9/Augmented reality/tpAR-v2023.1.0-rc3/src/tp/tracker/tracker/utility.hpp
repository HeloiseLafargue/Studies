#pragma once

#include "Camera.hpp"
#include <iostream>

#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>

#define DEBUGGING 1

#if DEBUGGING
#define PRINTVAR(a) std::cout << #a << " = " << (a) << endl << endl;
#else
#define PRINTVAR(a)
#endif

// Enumerative type containing the possible patterns for the chessboard

enum Pattern
{
    CHESSBOARD,
    CIRCLES_GRID,
    ASYMMETRIC_CIRCLES_GRID
};

/**
 * Detect a chessboard in a given image
 *
 * @param[in] rgbimage The rgb image to process
 * @param[out] pointbuf the set of 2D image corner detected on the chessboard
 * @param[in] boardSize the size of the board in terms of corners (width X height)
 * @param[in] patternType The type of chessboard pattern to look for
 * @return true if the chessboard is detected inside the image, false otherwise
 */
bool detectChessboard(const cv::Mat& rgbimage,
                      std::vector<cv::Point2f>& pointbuf,
                      const cv::Size& boardSize,
                      Pattern patternType);

/**
 * Decompose the homography into its components R and t
 *
 * @param[in] H The homography H = [h1 h2 h3]
 * @param[in] matK The 3x3 calibration matrix K
 * @param[out] poseMat the 3x4 pose matrix [R t]
 */
void decomposeHomography(const cv::Mat& H, const cv::Mat& matK, cv::Mat& poseMat);

/**
 *
 * @param[in,out] rgbimage The image on which to draw the reference system
 * @param[in] cam The camera
 * @param[in] poseMat The pose of the camera
 * @param[in] thickness The thickness of the line
 * @param[in] scale A scale factor for the unit vectors to draw
 * @param[in] alreadyUndistorted A boolean value that tells if the input image rgbimage is already undistorted or we are
 * working on a distorted image
 */
void drawReferenceSystem(cv::Mat& rgbimage,
                         const Camera& cam,
                         const cv::Mat& poseMat,
                         int thickness,
                         double scale,
                         bool alreadyUndistorted = true);

/**
 * Wrapper around the original opencv's projectPoints
 *
 * @param[in] objectPoints the 3D points
 * @param[in] poseMat the pose matrix of the camera
 * @param[in] cameraMatrix the calibration matrix
 * @param[in] distCoeffs the distortion coefficients
 * @param[out] imagePoints the projected points
 */
void myProjectPoints(cv::InputArray objectPoints,
                     const cv::Mat& poseMat,
                     cv::InputArray cameraMatrix,
                     cv::InputArray distCoeffs,
                     cv::OutputArray imagePoints);

/**
 * Wrapper around the original opencv's solvePnPRansac
 *
 * @param[in] objectPoints the 3D points
 * @param[in] imagePoints the image points
 * @param[in] cameraMatrix the calibration matrix
 * @param[in] distCoeffs the distortion coefficients
 * @param[out] poseMat the pose matrix
 * @param[out] inliers the list of indices of the inliers points
 */
void mySolvePnPRansac(cv::InputArray objectPoints,
                      cv::InputArray imagePoints,
                      cv::InputArray cameraMatrix,
                      cv::InputArray distCoeffs,
                      cv::Mat& poseMat,
                      cv::OutputArray inliers = cv::noArray());

/**
 * Generate the set of 3D points of a chessboard
 *
 * @param[in] boardSize the size of the board in terms of corners (width X height)
 * @param[in] squareSize the size in mm of the each square of the chessboard
 * @param[out] corners the set of 2D points on the chessboard
 * @param[in] patternType The type of chessboard pattern to look for
 */
void calcChessboardCorners(const cv::Size& boardSize,
                           float squareSize,
                           std::vector<cv::Point2f>& corners,
                           Pattern patternType = CHESSBOARD);

/**
 * Generate the set of 3D points of a chessboard
 *
 * @param[in] boardSize the size of the board in terms of corners (width X height)
 * @param[in] squareSize the size in mm of the each square of the chessboard
 * @param[out] corners the set of 3D points on the chessboard
 * @param[in] patternType The type of chessboard pattern to look for
 */
void calcChessboardCorners3D(const cv::Size& boardSize,
                             float squareSize,
                             std::vector<cv::Point3f>& corners,
                             Pattern patternType = CHESSBOARD);

/**
 * Filter a generic vector against a list of index of the elements to be deleted,
 *
 * @param[in,out] inout the vector to filter
 * @param[in] idx list of indices of the element to keep
 */
template<typename T> void filterVector(std::vector<T>& inout, const std::vector<int>& idx)
{
    std::vector<T> temp;
    temp.reserve(idx.size());

    for(const auto& i : idx)
    {
        assert(i < inout.size());
        temp.push_back(inout[i]);
    }

    inout.clear(); // necessary??
    inout = temp;
}

/**
 * @brief Utility function used to detect the size and the type of a video stream.
 * The video must be already open.
 *
 * @param[in] videoFilename The name of the video file
 * @paramp[in,out] capture The video stream, it must be already open
 * @param[out] singleSize The size of the frames (width x height) of the video
 * @param[out] imgInType The type of the frame (uchar, int etc)
 * @return true if everything went ok
 */
bool getVideoSizeAndType(const std::string& videoFilename,
                         cv::VideoCapture capture,
                         cv::Size& singleSize,
                         int& imgInType);
