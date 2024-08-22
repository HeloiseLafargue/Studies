#include <iostream>

#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>

using namespace cv;
using namespace std;

int main(int argc, char** argv)
{
    if(argc != 2)
    {
        cout << " Usage: display_video VideoToLoadAndDisplay" << endl;
        return EXIT_FAILURE;
    }
    Mat image;
    // Read the file
    VideoCapture capture;
    capture.open(argv[1]);
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
