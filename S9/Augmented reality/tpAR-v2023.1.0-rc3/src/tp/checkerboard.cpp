#include "tracker/utility.hpp"

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>

#include <cstdio>
#include <cstring>
#include <iostream>
#include <ctime>

using namespace cv;
using namespace std;


// Display the help for the program
void help( const char* programName );

// parse the input command line arguments
bool parseArgs( int argc, char**argv, Size &boardSize, string &inputFilename, Pattern &pattern );

int main( int argc, char** argv )
{
    /******************************************************************/
    /* CONSTANTS to use                                               */
    /******************************************************************/

    // the name of the window
    const string WINDOW_NAME = "Image View";


    /******************************************************************/
    /* VARIABLES TO use                                               */
    /******************************************************************/

    Mat view; // it will contain the original image loaded from file

    // it will contain the detected corners on the chessboard
    vector<Point2f> pointbuf;

    // it will be true if a chessboard is found, false otherwise
    bool found;

    // it will contain the size in terms of corners (width X height) of the chessboard
    Size boardSize;

    // it will contains the filename of the image file
    string inputFilename;

    // Default pattern is chessboard
    Pattern pattern = CHESSBOARD;



    /******************************************************************/
    /* READ THE INPUT PARAMETERS - DO NOT MODIFY                      */
    /******************************************************************/

    if( !parseArgs( argc, argv, boardSize, inputFilename, pattern ) )
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
    // read the input image from file into "view" --> see imread
    /******************************************************************/
    view = imread(inputFilename, IMREAD_COLOR); 


    //Measure the execution time, get time before function call
    auto t = ( double ) getTickCount( );

    /******************************************************************/
    // call the function that detects the chessboard on the image
    // found = detectChessboard...
    /******************************************************************/
    found = detectChessboard(view, pointbuf, boardSize, pattern);

    // get time after function call and display info
    t = ( ( double ) getTickCount( ) - t ) / getTickFrequency( );

    cout << ( ( !found ) ? ( "No " ) : ( "" ) ) << "chessboard detected!" << endl;
    cout << "Chessboard detection took " << t * 1000 << "ms" << endl;


    /******************************************************************/
    // if the chessboard is found draw the cornerns on top of it
    // --> see drawChessboardCorners
    /******************************************************************/
    drawChessboardCorners(view, boardSize, pointbuf, found);

    /******************************************************************/
    // show the image inside the window --> see imshow
    /******************************************************************/
    imshow(WINDOW_NAME, view);

    // wait for user input before ending --> see waitKey
    waitKey( -1 );


    return EXIT_SUCCESS;
}


// Display the help for the programm

void help( const char* programName )
{
    cout << "Detect a chessboard in a given image" << endl
            << "Usage: " << programName << endl
            << "     -w <board_width>                                  # the number of inner corners per one of board dimension" << endl
            << "     -h <board_height>                                 # the number of inner corners per another board dimension" << endl
            << "     [-pt <pattern=[circles|acircles|chess]>]          # the type of pattern: chessboard or circles' grid" << endl
            << "     <image file> " << endl
            << endl;
}

// parse the input command line arguments

bool parseArgs( int argc, char**argv, Size &boardSize, string &inputFilename, Pattern &pattern )
{
    // check the minimum number of arguments
    if( argc < 3 )
    {
        help( argv[0] );
        return false;
    }


    // Read the input arguments
    for( int i = 1; i < argc; i++ )
    {
        const char* s = argv[i];
        if( strcmp( s, "-w" ) == 0 )
        {
            if( sscanf( argv[++i], "%u", &boardSize.width ) != 1 || boardSize.width <= 0 )
            {
                cerr << "Invalid board width" << endl;
                return false;
            }
        }
        else if( strcmp( s, "-h" ) == 0 )
        {
            if( sscanf( argv[++i], "%u", &boardSize.height ) != 1 || boardSize.height <= 0 )
            {
                cerr << "Invalid board height" << endl;
                return false;
            }
        }
        else if( s[0] != '-' )
        {
            inputFilename.assign( s );
        }
        else if( strcmp( s, "-pt" ) == 0 )
        {
            i++;
            if( !strcmp( argv[i], "circles" ) )
                pattern = CIRCLES_GRID;
            else if( !strcmp( argv[i], "acircles" ) )
                pattern = ASYMMETRIC_CIRCLES_GRID;
            else if( !strcmp( argv[i], "chess" ) )
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

