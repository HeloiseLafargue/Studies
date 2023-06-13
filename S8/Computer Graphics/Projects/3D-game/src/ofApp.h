#pragma once

#include "ofMain.h"
#include "Game.h"
#include "State.h"
#include "ofxGui.h"

class ofApp : public ofBaseApp{

	public:
        ~ofApp();
		void setup();
		void update();
		void draw();

		void keyPressed(int key);
		void keyReleased(int key);
		
        Game *game;
};
