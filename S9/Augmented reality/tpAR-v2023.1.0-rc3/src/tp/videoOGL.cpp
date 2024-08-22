#include "tracker/utility.hpp"

#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#ifdef __APPLE__
#include <OpenGL/gl.h>
// #include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#ifdef _WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
// #include <GL/glu.h>
#include <GL/freeglut.h>
#endif

#include <chrono>
#include <cstdlib>
#include <iostream>
#include <thread>

using namespace std;
using namespace cv;

/*
 * Common globals
 */

int gFinished;

// the OpenGL reference for the texture to display
GLuint gCameraTextureId;

// this will physically contain the current frame that is used as texture
Mat gResultImage;

// the size of the video frame
Size singleSize;

// OpenGL initialization

void glInit()
{
    glEnable(GL_TEXTURE_2D);
    glGenTextures(1, &gCameraTextureId);
}

// Updates texture handle gCameraTextureId with OpenCV image in cv::Mat from gResultImage

void updateTexture()
{
    glBindTexture(GL_TEXTURE_2D, gCameraTextureId);

    // set texture filter to linear - we do not build mipmaps for speed
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    // create the texture from OpenCV image data
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_RGB8,
                 singleSize.width,
                 singleSize.height,
                 0,
#if _WIN32
                 GL_BGR_EXT,
#else
                 GL_BGR,
#endif
                 GL_UNSIGNED_BYTE,
                 gResultImage.data);
}

/**
 * Draw the background from the camera image
 */
void drawBackground()
{
    // set up the modelview matrix so that the view is between [-1,-1] and [1,1]
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-1, 1, -1, 1, 0, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // draw the quad textured with the camera image
    glBindTexture(GL_TEXTURE_2D, gCameraTextureId);
    glBegin(GL_QUADS);
    glTexCoord2f(.0f, 1.f);
    glVertex2f(-1.f, -1.f);
    glTexCoord2f(.0f, .0f);
    glVertex2f(-1.f, 1.f);
    glTexCoord2f(1.f, .0f);
    glVertex2f(1.f, 1.f);
    glTexCoord2f(1.f, 1.f);
    glVertex2f(1.f, -1.f);
    glEnd();

    // reset the projection matrix
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
}

/** OpenGL display callback */
void displayFunc()
{
    glClear(GL_COLOR_BUFFER_BIT);

    // render the background image from camera texture
    glEnable(GL_TEXTURE_2D);

    drawBackground();

    // clear th depth buffer bit so that the background is overdrawn
    glClear(GL_DEPTH_BUFFER_BIT);

    // everything will be white
    glColor3f(1, 1, 1);

    // start with fresh modelview matrix and apply the transform of the plane
    glLoadIdentity();

    // enable the texture for a nice effect ;)
    glDisable(GL_TEXTURE_2D);

    glutSwapBuffers();
    glutPostRedisplay();
}

// Windows resize callback

void reshape(GLint width, GLint height) { glViewport(0, 0, width, height); }

// Keyboard callback

void keyFunc(unsigned char key, int x, int y)
{
    cout << key << " pressed" << endl;

    switch(key)
    {
        case 27: gFinished = true; break;

        default: break;
    }
}

void printHelp(const string& name)
{
    cout << "Usage: " << endl << "\t" << name << " <videofile.avi> " << endl << endl << "Options: " << endl;
    cout << endl;
}

int main(int argc, char** argv)
{
    string videoFilename;
    int imgInType;

    long frameNumber = 0;

    if(argc < 2)
    {
        cerr << "Wrong number of parameters" << endl;
        printHelp(string(argv[0]));
        return EXIT_FAILURE;
    }

    VideoCapture capture;

    videoFilename.assign(argv[1]);

    capture.open(videoFilename);

    // check if capture has opened the video
    if(!capture.isOpened())
    {
        cerr << "Could not open video file " << videoFilename << endl;
        return EXIT_FAILURE;
    }
    if(!getVideoSizeAndType(videoFilename, capture, singleSize, imgInType))
    {
        cerr << "Something wrong while checking the size and type of the video " << videoFilename << endl;
        return EXIT_FAILURE;
    }

    gResultImage = Mat(singleSize, imgInType);

    // Setup GLUT rendering and callbacks
    glutInit(&argc, argv);
    glutCreateWindow("Main");
    glutKeyboardFunc(keyFunc);
    glutReshapeFunc(reshape);
    // reshape the window with the size of the image
    glutReshapeWindow(singleSize.width, singleSize.height);
    glutDisplayFunc(displayFunc);

    glInit();

    gFinished = false;

    while(!gFinished)
    {
        Mat view0;
        capture >> view0;

        // get a copy of the frame
        if(view0.empty())
        {
            cerr << "no more images available" << endl;
            gFinished = true;
            break;
        }

        view0.copyTo(gResultImage);

        cout << endl << endl << "****************** frame " << frameNumber << " ******************" << endl;

        ++frameNumber;

        // update the texture to be displayed in OPENGL
        updateTexture();

        // force Opengl to call the displayFunc
#if __APPLE__
        glutCheckLoop();
#else
        glutMainLoopEvent();
#endif

        // sleep for 35ms
        std::this_thread::sleep_for(std::chrono::milliseconds(35));
    }

    capture.release();

    return EXIT_SUCCESS;
}
