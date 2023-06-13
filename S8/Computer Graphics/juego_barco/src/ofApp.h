#pragma once

#include "ofMain.h"
#include "ofxGui.h""
class MyObj {
public:
	float x;
	float y;
	int randColor;
};

class ofApp : public ofBaseApp{

		enum States {
			arriba, cae, abajo
		};

	public:
		void setup();
		void update();
		void draw();

		void keyPressed(int key);
		void keyReleased(int key);
		void mouseMoved(int x, int y );
		void mouseDragged(int x, int y, int button);
		void mousePressed(int x, int y, int button);
		void mouseReleased(int x, int y, int button);
		void mouseEntered(int x, int y);
		void mouseExited(int x, int y);
		void windowResized(int w, int h);
		void dragEvent(ofDragInfo dragInfo);
		void gotMessage(ofMessage msg);	

		MyObj object;
		States state, prevState;
		string s;
		float waitingTime;
		ofImage img;
		ofxPanel gui;
};


