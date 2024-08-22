#include "tracker/Camera.hpp"
#include "tracker/ChessboardCameraTrackerKLT.hpp"
#include "tracker/utility.hpp"
#include <cstdio>
#include <cstring>
#include <ctime>
#include <iostream>

#include <opencv2/highgui/highgui.hpp>

using namespace cv;
using namespace std;

// Display the help for the program
void help(const char* programName);

// parse the input command line arguments
bool parseArgs(int argc, char** argv, Size& boardSize, string& inputFilename, string& calibFile);

int main(int argc, char** argv)
{
    /******************************************************************/
    /* CONSTANTS to use                                               */
    /******************************************************************/

    // the name of the window
    const string WINDOW_NAME = "Image View";

    /******************************************************************/
    /* VARIABLES to use                                               */
    /******************************************************************/

    // it will contain the size in terms of corners (width X height) of the chessboard
    Size boardSize;

    // it will contain the filename of the image file
    string inputFilename;

    // it will contain the filename of the image file
    string calibFilename;

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

    /******************************************************************/
    /* READ THE INPUT PARAMETERS - DO NOT MODIFY                      */
    /******************************************************************/
    if(!parseArgs(argc, argv, boardSize, inputFilename, calibFilename))
    {
        cerr << "Aborting..." << endl;
        return EXIT_FAILURE;
    }

    /******************************************************************/
    /* PART TO DEVELOP                                                  */
    /******************************************************************/

    //******************************************************************/
    // create a window using WINDOW_NAME as name to display the image --> see namedWindow
    //******************************************************************/
    namedWindow(WINDOW_NAME, CV_WINDOW_AUTOSIZE | CV_WINDOW_KEEPRATIO);

    //******************************************************************/
    // read the input video with capture (same as before)
    //******************************************************************/
    capture.open(string(inputFilename));

    //******************************************************************/
    // check it is really opened
    //******************************************************************/
    if(!capture.isOpened())
    {
        cerr << "Could not open video file " << inputFilename << endl;
        return EXIT_FAILURE;
    }

    //******************************************************************/
    // init the Camera loading the calibration parameters
    //******************************************************************/


    // processing loop
    while(true)
    {
        Mat view;
        /******************************************************************/
        // get the new frame from capture and copy it to view
        /******************************************************************/


        /******************************************************************/
        // if no more images to process exit the loop
        /******************************************************************/



        //******************************************************************/
        // process the image
        //******************************************************************/
        //if...
        {
            //******************************************************************/
            // draw the reference on top of the image
            //******************************************************************/

        }

        /******************************************************************/
        // show the image inside the window --> see imshow
        /******************************************************************/


        // wait 20ms for user input before processing the next frame
        // Any user input will stop the execution
#if DEBUGGING
        if((waitKey(-1) & 0xff) == 'q')
            break;
#else
        if(waitKey(10) >= 0)
            break;
#endif
    }

    /******************************************************************/
    // release the video resource
    /******************************************************************/
    capture.release();

    return EXIT_SUCCESS;
}

// Display the help for the program

void help(const char* programName)
{
    cout
      << "Detect and track a chessboard with the KLT algorithm and display an augmented reference system on top of it"
      << endl
      << "Usage: " << programName << endl
      << "     -w <board_width>                                  # the number of inner corners per one of board "
         "dimension"
      << endl
      << "     -h <board_height>                                 # the number of inner corners per another board "
         "dimension"
      << endl
      << "     -c <calib file>                                   # the name of the calibration file" << endl
      << "     <video file>                                      # the name of the video file" << endl
      << endl;
}

// parse the input command line arguments

bool parseArgs(int argc, char** argv, Size& boardSize, string& inputFilename, string& calibFile)
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
        else
        {
            cerr << "Unknown option " << s << endl;
            return false;
        }
    }

    return true;
}
