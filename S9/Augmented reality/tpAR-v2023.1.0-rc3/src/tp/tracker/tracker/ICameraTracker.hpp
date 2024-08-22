#pragma once

#include "Camera.hpp"
#include "utility.hpp"

class ICameraTracker
{
  public:
    /***************************************************
     *               INTERFACES
     ***************************************************/

    /**
     *
     * @param input
     * @param pose
     * @param cam
     * @param boardSize
     * @param patt
     * @return
     */
    virtual bool process(
      cv::Mat& input, cv::Mat& pose, const Camera& cam, const cv::Size& boardSize, const Pattern& patt) = 0;

    /***************************************************
     *               METHODS
     ***************************************************/

    /**
     * Return the current projection matrix of the camera
     * @return current projection matrix of the camera
     */
    inline const cv::Mat& getCurrPose() const { return _currPose; }

  protected:
    /// 4x4 rototranslation matrix for the camera position
    cv::Mat _currPose;
};