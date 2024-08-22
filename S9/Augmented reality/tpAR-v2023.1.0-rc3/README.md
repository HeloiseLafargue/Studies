An Augmented Reality application
===========================================

------------
Introduction
------------

The objective of these TP sessions is to build a simple augmented reality application using OpenCV for the camera tracking part and OpenGL for the rendering. The exercises that will be proposed are meant to gradually introduce you to the OpenCV libraries and build all the function that are needed for a camera tracker in an incremental way, so that you will be able to test and debug each function, before plugging all together in the camera tracker class.
The main idea is to use a chessboard as a marker and render on top of that the augmented reality. The final application should render an OpenGL teapot on top of the chessboard. We will get there step by step, first detecting the chessboard, then create some basic rendering in OpenCV, and at last plugging everything in the OpenGL pipeline.

-----------------
Code Organization
-----------------
The directories are organized as follows:

* ``doc`` contains a pdf copy of this text, a pdf copy of the reduced opencv tutorials, a pdf copy of the full, original opencv tutorial, and a pdf copy of the API reference manual of opencv (ie the copy of the online documentation). 

* ``data`` contains some images, videos and other data that will be used through the TP. 

* ``src`` contains the source files that you have to modify and complete; they are organize in directories:

	* ``tutorials`` contains the code used in the tutorials, in case you need to try it;
	* ``tp`` contains the files that you need to modify and complete through all the sessions of the TP.

* ``3dparty`` this directory contains a library that will be used at the end of the TP. You don't have to worry (or do anything...) about this directory.


--------
Building
--------

See [BUILD](BUILD.md) text file


-------
License
-------

See [LICENSE](LICENSE) text file

-------
Authors
-------

Simone Gasparini


---------
Contact
---------

Simone Gasparini simone.gasparini@enseeiht.fr