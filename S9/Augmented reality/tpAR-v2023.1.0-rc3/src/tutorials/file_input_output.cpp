#include <iostream>
#include <string>

#include <opencv2/core/core.hpp>

using namespace cv;
using namespace std;

static void help(char** av)
{
    cout << endl
         << av[0] << " shows the usage of the OpenCV serialization functionality." << endl
         << "usage: " << endl
         << av[0] << " outputfile.yml.gz" << endl
         << "The output file may be either XML (xml) or YAML (yml/yaml). You can even compress it by "
         << "specifying this in its extension like xml.gz yaml.gz etc... " << endl;
}

int main(int ac, char** av)
{
    if(ac != 2)
    {
        help(av);
        return EXIT_FAILURE;
    }

    string filename = av[1];
    {
        // write
        Mat R = Mat(3, 3, CV_8UC3);
        // fill the matrix with uniformly-distributed random values
        randu(R, Scalar::all(0), Scalar::all(255));

        Mat T = Mat(3, 1, CV_32FC3);
        // fill the matrix with normally distributed random values
        randn(T, Scalar::all(0), Scalar::all(1));

        FileStorage fs(filename, FileStorage::WRITE);

        fs << "R" << R; // cv::Mat
        fs << "T" << T;

        // explicit close
        fs.release();
        cout << "Write Done." << endl;
    }

    {
        // read
        cout << endl << "Reading: " << endl;
        FileStorage fs;
        fs.open(filename, FileStorage::READ);

        if(!fs.isOpened())
        {
            cerr << "Failed to open " << filename << endl;
            help(av);
            return EXIT_FAILURE;
        }

        Mat R, T;

        // Read cv::Mat
        fs["R"] >> R;
        fs["T"] >> T;

        cout << endl << "R = " << R << endl;
        cout << "T = " << T << endl << endl;
    }

    cout << endl << "Tip: Open up " << filename << " with a text editor to see the serialized data." << endl;

    return EXIT_SUCCESS;
}
