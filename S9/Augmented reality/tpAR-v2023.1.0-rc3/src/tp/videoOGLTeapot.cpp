#include "tracker/Camera.hpp"
#include "tracker/ChessboardCameraTracker.hpp"
#include "tracker/ChessboardCameraTrackerKLT.hpp"
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
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <thread>

#include <glm.h>

using namespace std;
using namespace cv;

// Display the help for the program
void help(const char* programName);

// parse the input command line arguments
bool parseArgs(int argc, char** argv, Size& boardSize, string& inputFilename, string& calibFile, string& objFile);

/*
 * Common globals
 */

int gFinished;

// the OpenGL reference for the texture to display
GLuint gCameraTextureId;

// the global array containing the explicit modelview matrix
const float* gModelViewMatrix;

// the global array containing the explicit projection matrix
float gProjectionMatrix[16] = {0.f};

// this will physically contain the current frame that is used as texture
Mat gResultImage;
bool stop = false;

// the size of the video frame
Size singleSize;

// OpenGL initialization

void glInit()
{
    //******************************************************************
    // enable the depth test
    //******************************************************************



    glEnable(GL_TEXTURE_2D);
    glGenTextures(1, &gCameraTextureId);

    //******************************************************************
    // set the Gouraud shading
    //******************************************************************


    //******************************************************************
    // set the LIGHT0 as a simple white, directional light with direction [1,2,-2]
    //******************************************************************



    //******************************************************************
    // set the material properties for the teapot
    // choose the values for ambient, diffuse,  specular and shininess
    // as you prefer. The teapot in the figure has is mainly gray with
    // ambient 0.7, diffuse 0.8, specular 1.0 and shininess 100
    //******************************************************************










    //******************************************************************
    // enable the lights
    //******************************************************************



    //******************************************************************
    // set the opengl projection matrix to gProjectionMatrix:
    // load the identity and multiply it by gProjectionMatrix using glMultMatrixf
    //******************************************************************




    //******************************************************************
    // set back the modelview mode
    //******************************************************************

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
        glTexCoord2f(0, 1);
        glVertex2f(-1, -1);
        glTexCoord2f(0, 0);
        glVertex2f(-1, 1);
        glTexCoord2f(1, 0);
        glVertex2f(1, 1);
        glTexCoord2f(1, 1);
        glVertex2f(1, -1);
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
    //******************************************************************
    // disable the lighting before drawing the background
    //******************************************************************


    drawBackground();

    // clear th depth buffer bit so that the background is overdrawn
    glClear(GL_DEPTH_BUFFER_BIT);

    // everything will be white
    glColor3f(1, 1, 1);

    // start with fresh modelview matrix and apply the transform of the plane
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    //******************************************************************
    // apply the modelview matrix gModelViewMatrix using glMultMatrixf
    //******************************************************************


    // enable the texture for a nice effect ;)
    glDisable(GL_TEXTURE_2D);

    //******************************************************************
    // enable the lighting before drawing the teapot/the object
    //******************************************************************


    //******************************************************************
    // draw the teapot (the solid version)
    //******************************************************************


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

        case 's': stop = !stop; break;
        default: break;
    }
}

int main(int argc, char** argv)
{
    /******************************************************************/
    /* VARIABLES TO USE                                                  */
    /******************************************************************/

    // it will contain the size in terms of corners (width X height) of the chessboard
    Size boardSize;

    // Default pattern is chessboard
    Pattern pattern = CHESSBOARD;

    // the camera
    Camera cam;

    // the filenames for the video and the calibration
    string videoFilename, calibFilename, objFile;
    int imgInType;

    long frameNumber = 0;

    // the video capture
    VideoCapture capture;

    // the video capture a dummy matrix used to show the teapot in a fix position of the image
    Mat dummyMatrix = Mat::eye(4, 4, CV_32F);
    dummyMatrix.at<float>(1, 1) = -1;
    dummyMatrix.at<float>(2, 3) = 50;

    cout << dummyMatrix << endl;

    /******************************************************************/
    /* READ THE INPUT PARAMETERS - DO NOT MODIFY                      */
    /******************************************************************/

    if(!parseArgs(argc, argv, boardSize, videoFilename, calibFilename, objFile))
    {
        cerr << "Aborting..." << endl;
        return EXIT_FAILURE;
    }

    //******************************************************************
    // init the Camera loading the calibration parameters
    //******************************************************************


    //******************************************************************
    // get the corresponding projection matrix in OGL format
    //******************************************************************


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
        if(!stop)
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

            // set the gModelViewMatrix with the content of the dummy matrix
            // OpenGL uses a column-major order for storing the matrix element, while OpenCV uses
            // a row major order for storing the elements. Hence we need first to convert the dummy matrix
            // to its transpose and only then pass the data pointer to gModelViewMatrix
            gModelViewMatrix = (float*)Mat(dummyMatrix.t()).data;

            cout << endl << endl << "****************** frame " << frameNumber << " ******************" << endl;

            ++frameNumber;
        }

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

// Display the help for the programm

void help(const char* programName)
{
    cout << "Detect a chessboard in a given video and visualize a teapot on top of it" << endl
         << "Usage: " << programName << endl
         << "     -w <board_width>                                  # the number of inner corners per one of board "
            "dimension"
         << endl
         << "     -h <board_height>                                 # the number of inner corners per another board "
            "dimension"
         << endl
         << "     -c <calib file>                                   # the name of the calibration file" << endl
         << "     [-o <obj file>]                                   # the optional obj file containing the 3D model to "
            "display"
         << endl
         << "     <video file>                                      # the name of the video file" << endl
         << endl;
}

// parse the input command line arguments

bool parseArgs(int argc, char** argv, Size& boardSize, string& inputFilename, string& calibFile, string& objFile)
{
    // check the minimum number of arguments
    if(argc < 3)
    {
        help(argv[0]);
        return false;
    }

    // Read the input arguments
    for(int i = 1; i < argc; i++)
    {
        const char* s = argv[i];

        if(strcmp(s, "-w") == 0)
        {
            if(sscanf(argv[++i], "%u", &boardSize.width) != 1 || boardSize.width <= 0)
            {
                cerr << "Invalid board width" << endl;
                return false;
            }
        }
        else if(strcmp(s, "-h") == 0)
        {
            if(sscanf(argv[++i], "%u", &boardSize.height) != 1 || boardSize.height <= 0)
            {
                cerr << "Invalid board height" << endl;
                return false;
            }
        }
        else if(s[0] != '-')
        {
            inputFilename.assign(s);
        }
        else if(strcmp(s, "-c") == 0)
        {
            if(i + 1 < argc)
                calibFile.assign(argv[++i]);
            else
            {
                cerr << "Missing argument for option " << s << endl;
                return false;
            }
        }
        else if(strcmp(s, "-o") == 0)
        {
            if(i + 1 < argc)
                objFile.assign(argv[++i]);
            else
            {
                cerr << "Missing argument for the obj file " << s << endl;
                return false;
            }
        }
        else
        {
            cerr << "Unknown option " << s << endl;
            return false;
        }
    }

    return true;
}
