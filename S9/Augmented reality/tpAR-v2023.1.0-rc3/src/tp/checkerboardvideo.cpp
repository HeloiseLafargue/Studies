#include <cstdio>
#include <cstring>
#include <ctime>
#include <iostream>

#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <tracker/utility.hpp>

using namespace cv;
using namespace std;

// Display the help for the program
void help(const char* programName);

// parse the input command line arguments
bool parseArgs(int argc, char** argv, Size& boardSize, string& inputFilename, Pattern& pattern);

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

    vector<Point2f> pointbuf; // it will contain the detected corners on the chessboard

    bool found; // it will be true if a chessboard is found, false otherwise

    // it will contain the size in terms of corners (width X height) of the chessboard
    Size boardSize;

    // it will contains the filename of the image file
    string inputFilename;

    // Default pattern is chessboard
    Pattern pattern = CHESSBOARD;

    // Used to load the video and get the frames
    VideoCapture capture;

    /******************************************************************/
    /* READ THE INPUT PARAMETERS - DO NOT MODIFY                      */
    /******************************************************************/

    if(!parseArgs(argc, argv, boardSize, inputFilename, pattern))
    {
        cerr << "Aborting..." << endl;
        return EXIT_FAILURE;
    }

    /******************************************************************/
    /* PART TO DEVELOP                                                  */
    /******************************************************************/

    /******************************************************************/
    // create a window to display the image --> see namedWindow
    /******************************************************************/
    namedWindow(WINDOW_NAME, CV_WINDOW_AUTOSIZE);


    /******************************************************************/
    // read the input video with capture
    /******************************************************************/
    capture.open(inputFilename);

    /******************************************************************/
    // check it is really opened
    /******************************************************************/
    if (!capture.open(inputFilename)) {
        cerr << "Error: Unable to open video file." << endl;
        return EXIT_FAILURE;
    }


    // processing loop
    while(true)
    {
        Mat view;
        /******************************************************************/
        // get the new frame from capture and copy it to view
        /******************************************************************/
        capture.read(view);

        /******************************************************************/
        // if no more images to process exit the loop
        /******************************************************************/
        if (!capture.read(view)) {
            break;
        }

        // Measure the execution time, get time before function call
        auto t = (double)getTickCount();

        /******************************************************************/
        // call the function that detects the chessboard on the image
        // found = detectChessboard...
        /******************************************************************/
        found = detectChessboard(view, pointbuf, boardSize, pattern);

        // get time after function call and display info
        t = ((double)getTickCount() - t) / getTickFrequency();

        cout << ((!found) ? ("No ") : ("")) << "chessboard detected!" << endl;
        cout << "Chessboard detection took " << t * 1000 << "ms" << endl;

        /******************************************************************/
        // if the chessboard is found draw the corners on top of it
        // --> see drawChessboardCorners
        /******************************************************************/
        if (found) {
            drawChessboardCorners(view, boardSize, Mat(pointbuf), found);

        /******************************************************************/
        // show the image inside the window --> see imshow
        /******************************************************************/
            imshow(WINDOW_NAME, view);
        }


        // wait 20ms for user input before processing the next frame
        // Any user input will stop the execution
        if( waitKey( 10 ) >= 0)
            break;
    }

    /******************************************************************/
    // release the video resource
    /******************************************************************/


    return EXIT_SUCCESS;
}

// Display the help for the program

void help(const char* programName)
{
    cout << "Detect a chessboard in a given video" << endl
         << "Usage: " << programName << endl
         << "     -w <board_width>                                  # the number of inner corners per one of board "
            "dimension"
         << endl
         << "     -h <board_height>                                 # the number of inner corners per another board "
            "dimension"
         << endl
         << "     [-pt <pattern=[circles|acircles|chess]>]          # the type of pattern: chessboard or circles' grid"
         << endl
         << "     <video file> " << endl
         << endl;
}

// parse the input command line arguments

bool parseArgs(int argc, char** argv, Size& boardSize, string& inputFilename, Pattern& pattern)
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
        else if(strcmp(s, "-pt") == 0)
        {
            i++;
            if(!strcmp(argv[i], "circles"))
                pattern = CIRCLES_GRID;
            else if(!strcmp(argv[i], "acircles"))
                pattern = ASYMMETRIC_CIRCLES_GRID;
            else if(!strcmp(argv[i], "chess"))
                pattern = CHESSBOARD;
            else
            {
                cerr << "Invalid pattern type: must be chessboard or circles" << endl;
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
