# An Augmented Reality application

## Building instructions

Required tools:
* C/C++ compiler (gcc >= 4.6 or visual studio or clang)
* cmake
* [optional] Doxygen and graphviz to generate the htlm documentation


###  Dependencies

The project depends on:

- OpenCV 2.4.9
- OpenGL
- GLM ([webpage](http://devernay.free.fr/hacks/glm/)) which provides a more complete interface to the OBJ format (i.e. it manages textures, materials etc.).

If at any stage you get an error about missing ``Xi`` and ``Xmu`` library, you need to install them. In linux you can do

```shell
sudo apt-get install libxi-dev libxmu-dev.
```

###  Setting up and building:

OpenCV and OpenGL are normally already installed on your machine. In case you may have a look at the next section to install OpenCV on your own machine. As for GLM, it comes with the project, so it will be compiled and built the first time it is needed.

In order to setting up the project, from the root of the project do:

```shell
mkdir build
cd build
cmake .. 
```

Then each application can be compiled by simply doing a

```shell
make <filename_without_extension>
```

If you run
```shell
make help
```
a list of all possible targets is displayed. 

Remember to set the ``LD_LIBRARY_PATH`` to allow your application to access the ``GLM`` library:
```shell
export $LD_LIBRARY_PATH=/home/<yourhome>/<path/to/code>/3dparty/glm/build/lib:$LD_LIBRARY_PATH
```
Also you may want to add the path to OpenCV if they are not installed in the system.

Finally, as usual, running 

```shell
make clean
```

will delete all the executable and the compilation objects.



In case you want to try the code on a different machine, you need to first install the OpenCV libraries (see next section) and then do 

```shell
cmake .. -DOpenCV_DIR=path/to/OpenCVConfig.cmake 
```

in order to specify the directory where you build them. More in general, you need to provide the path to the file ``OpenCVConfig.cmake``, which you can find e.g. with 

```shell
locate OpenCVConfig.cmake 
```
from your shell.



#### Code Documentation

In order to generate the documentation you need have doxygen and graphviz installed. On linux:
```shell
sudo apt-get install doxygen graphviz.
```

On Mac OSX with homebrew
```shell
brew install doxygen graphviz.
```

Then a 
```shell
make doc
```
will generate the documentation in the ``doc`` folder of your build.



#### Installing OpenCV 

You can download the code from [here](http://opencv.org/downloads.html) or clone the [github repository](https://github.com/itseez/opencv)
Create a `build` directory where to build the library. It also advisable to set an non-system install directory, so that it will be easier to set up the environment later:

```
mkdir build && cd build && cmake .. -DCMAKE_INSTALL_PREFIX=`pwd`/install
make install -j n
```

You can even run ``ccmake ..`` to set up other options (eg. CUDA support).