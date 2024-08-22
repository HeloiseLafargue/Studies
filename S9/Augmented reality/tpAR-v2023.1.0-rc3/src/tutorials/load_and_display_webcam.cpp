#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>

#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>

using namespace cv;
using namespace std;

int main(int argc, char** argv)
{
    if(argc != 2 || !isdigit(*argv[1]))
    {
        cout << " Usage: " << argv[0] << " deviceNumber" << endl;
        return EXIT_FAILURE;
    }
    int cameraId = 0;
    sscanf(argv[1], "%d", &cameraId);
    cout << cameraId << endl;
    Mat image;
    // Read the file
    VideoCapture capture;
    capture.open(cameraId);
    // Check if the video is loaded
    if(!capture.isOpened())
    {
        cout << "Could not open or find the video" << std::endl;
        return EXIT_FAILURE;
    }

    cout << "FRAME_WIDTH: " << capture.get(CV_CAP_PROP_FRAME_WIDTH) << endl;
    cout << "FRAME_HEIGHT: " << capture.get(CV_CAP_PROP_FRAME_HEIGHT) << endl;
    cout << "FOURCC: " << capture.get(CV_CAP_PROP_FOURCC) << endl;
    cout << "FPS: " << capture.get(CV_CAP_PROP_FPS) << endl;
    // Create a window for display.

    capture.set(CV_CAP_PROP_FRAME_WIDTH, 640);
    capture.set(CV_CAP_PROP_FRAME_HEIGHT, 480);
    cout << capture.get(CV_CAP_PROP_FRAME_WIDTH) << endl;
    namedWindow("Display window", CV_WINDOW_AUTOSIZE);

    // infinite loop
    while(true)
    {
        capture >> image;

        // check if there are still frames
        if(image.empty())
            break;

        // Show our image inside it.
        imshow("Display window", image);

        if(waitKey(10) == 'q')
            break;
    }

    return EXIT_SUCCESS;
}
