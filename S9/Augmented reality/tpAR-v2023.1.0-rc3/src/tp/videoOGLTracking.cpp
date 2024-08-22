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
// the global array contain the explicit modelview matrix
const float* gModelViewMatrix;
// the global array contain the explicit projection matrix
float gProjectionMatrix[16] = {0.f};
// this will physically contain the current frame that is used as texture
Mat gResultImage;
bool stop = false;

// the size of the video frame
Size singleSize;

void DrawAxis(float scale)
{
    glPushMatrix();
    glDisable(GL_LIGHTING);
    //    glDisable(GL_TEXTURE_2D);
    glScalef(scale, scale, scale);
    glLineWidth(4.0);

    glBegin(GL_LINES);

    glColor3f(1.f, 0.f, 0.f);
    glVertex3f(.8f, .05f, 0.f);
    glVertex3f(1.f, .25f, 0.f); /*  Letter X */
    glVertex3f(.8f, .25f, 0.f);
    glVertex3f(1.f, .05f, 0.f);
    glVertex3f(0.f, 0.f, 0.f);
    glVertex3f(1.f, 0.f, 0.f); /*  X axis */

    glColor3f(0.f, 1.f, 0.f);
    glVertex3f(0.f, 0.f, 0.f);
    glVertex3f(0.f, 1.f, 0.f); /*  Y axis */

    glColor3f(0.f, 0.f, 1.f);
    glVertex3f(0.f, 0.f, 0.f);
    glVertex3f(0.f, 0.f, 1.f); /*  Z axis */
    glEnd();
    //    if (lighting)
    glEnable(GL_LIGHTING);
    //    if (lighting)
    //    glEnable(GL_TEXTURE_2D);
    glColor3f(1.f, 1.f, 1.f);
    glPopMatrix();
}

void DrawPoints(float scale)
{
    glPushMatrix();
    glDisable(GL_LIGHTING);
    //    glDisable(GL_TEXTURE_2D);
    glScalef(scale, scale, scale);
    glPointSize(5);
    glBegin(GL_POINTS);

    for(int i = 0; i < 6; i++)
    {
        glColor3f(1.f, (float)40.0f * i / 255.0f, (float)40.0f * i / 255.0f);
        for(int j = 0; j < 9; j++)
            glVertex3f(float(j), float(i), 0.f);
    }

    glEnd();
    //    if (lighting)
    glEnable(GL_LIGHTING);
    //    if (lighting)
    //    glEnable(GL_TEXTURE_2D);
    glColor3f(1.f, 1.f, 1.f);
    glPopMatrix();
}

/**
 * OpenGL initialization
 */
void glInit()
{
    //******************************************************************
    // enable the depth test
    //******************************************************************
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);

    glEnable(GL_TEXTURE_2D);
    glGenTextures(1, &gCameraTextureId);

    //******************************************************************
    // set the Gouraud shading
    //******************************************************************
    glShadeModel(GL_SMOOTH);

    //******************************************************************
    // set the LIGHT0 as a simple white, directional light with direction [1,2,-2]
    //******************************************************************
    GLfloat gLightPos[] = {100.f, 200.f, -200.f, 0.0};
    glLightfv(GL_LIGHT0, GL_POSITION, gLightPos);

    //******************************************************************
    // set the material properties for the teapot
    // choose the values for ambient, diffuse,  specular and shininess
    // as you prefer. The teapot in the figure has is mainly gray with
    // ambient 0.7, diffuse 0.8, specular 1.f and shininess 100
    //******************************************************************
    GLfloat mat_ambient[] = {0.7, 0.7, 0.7, 1.f};
    GLfloat mat_diffuse[] = {0.8, 0.8, 0.8, 1.f};
    GLfloat mat_specular[] = {1.f, 1.f, 1.f, 1.f};
    GLfloat high_shininess[] = {100.0};

    glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
    glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
    glMaterialfv(GL_FRONT, GL_SHININESS, high_shininess);

    //******************************************************************
    // enable the lights
    //******************************************************************
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    //******************************************************************
    // set the opengl projection matrix to gProjectionMatrix:
    // load the identity and multiply it by gProjectionMatrix using glMultMatrixf
    //******************************************************************
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glMultMatrixf(gProjectionMatrix);

    //******************************************************************
    // set back the modelview mode
    //******************************************************************
    glMatrixMode(GL_MODELVIEW);
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
    glDisable(GL_LIGHTING);

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
    glMultMatrixf(gModelViewMatrix);
//     DrawPoints(25);
    glRotatef(-90, 1, 0, 0);

    // enable the texture for a nice effect ;)
    glDisable(GL_TEXTURE_2D);
    //******************************************************************
    // enable the lighting before drawing the teapot/the object
    //******************************************************************
    glEnable(GL_LIGHTING);

//     DrawAxis(100);

    glTranslatef(0, 50, 0);
    //******************************************************************
    // draw the teapot (the solid version)
    //******************************************************************
    glutSolidTeapot(45);

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
    string videoFilename, calibFilename, objFile;
    int imgInType;

    long frameNumber = 0;

    /******************************************************************/
    /* VARIABLES TO USE                                                  */
    /******************************************************************/

    // it will contain the size in terms of corners (width X height) of the chessboard
    Size boardSize;

    // Default pattern is chessboard
    Pattern pattern = CHESSBOARD;

    // Used to load the video and get the frames
    VideoCapture capture;

    // Camera object containing the calibration parameters
    Camera cam;

    // Camera Tracker object
    ChessboardCameraTrackerKLT tracker;

    // 3x4 camera pose matrix [R t]
    Mat cameraPose;

    // Mat dummyMatrix = Mat::eye( 4, 4, CV_32F );
    // dummyMatrix.at<float>(0, 3) = 102;
    // dummyMatrix.at<float>(1, 3) = 46;
    // dummyMatrix.at<float>(2, 3) = 217;
    // Mat dummyMatrix = (Mat_<float>(4,4) << -0.90750873, -0.0011025554, 0, 125.93854, 0.39205164, -0.0022058936,
    // 0.00093782519, 43.355019, -0.15074302, 0.00085026468, 0.0024341263, 384.71075, 0,0,0,1);
    Mat dummyMatrix = (Mat_<float>(4, 4) << 0.4830,
                       -0.8756,
                       0.0077,
                       125.93854,
                       0.8365,
                       0.4588,
                       -0.2996,
                       43.355019,
                       0.2588,
                       0.1511,
                       0.9540,
                       384.71075,
                       0,
                       0,
                       0,
                       1);

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
    cam.init(calibFilename);

    //******************************************************************
    // get the corresponding projection matrix in OGL format
    //******************************************************************
    cam.getOGLProjectionMatrix(gProjectionMatrix, 10.f, 10000.f);

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

            //            undistort( view0, gResultImage, cam.matK, cam.distCoeff );

            // process the image
            if(tracker.process(view0, cameraPose, cam, boardSize, pattern))
            {
                Mat temp;
                cameraPose.convertTo(temp, CV_32F);
                PRINTVAR(temp);
#if CV_MINOR_VERSION < 4
                Mat fooMat = dummyMatrix.rowRange(0, 3);
                temp.copyTo(fooMat);
#else
                temp.copyTo(dummyMatrix.rowRange(0, 3));
#endif
                PRINTVAR(dummyMatrix);
                // gModelViewMatrix = (float*) Mat(temp.t()).data;
                gModelViewMatrix = (float*)Mat(dummyMatrix.t()).data;
            }

            view0.copyTo(gResultImage);
            // gModelViewMatrix = (float*) Mat(dummyMatrix.t()).data;

            // cout << endl << endl << "****************** frame " << frameNumber << " ******************" << endl;

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
         << "     -o <obj file>                                     # the obj file containing the 3D model to display"
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
