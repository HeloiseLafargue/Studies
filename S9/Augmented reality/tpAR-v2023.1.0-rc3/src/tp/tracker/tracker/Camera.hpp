#pragma once

#include <opencv2/core/core.hpp>

class Camera
{
  public:
    Camera() = default;

    /**
     * Initialize the camera loading the internal parameters from the given file
     *
     * @param[in] calibFilename the calibration file
     * @return true if success
     */
    bool init(const std::string& calibFilename);

    virtual ~Camera() = default;

    /**
     * Return the OpenGL projection matrix for the camera
     * @param[out] proj the OGL projection matrix (ready to be passed, ie in col major format)
     * @param znear near clipping plane
     * @param zfar far clipping plane
     * \note using http://strawlab.org/2011/11/05/augmented-reality-with-OpenGL/
     */
    void getOGLProjectionMatrix(float* proj, float znear, float zfar) const;

  public:
    cv::Mat matK;

    cv::Mat distCoeff;

    cv::Size imageSize;
};
