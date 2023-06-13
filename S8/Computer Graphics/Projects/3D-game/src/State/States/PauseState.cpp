#include "Game.h"
#include "PauseState.h"
#include "MenuState.h"
#include "PlayState.h"

 
PauseState::PauseState(Game *game): State(game, "Pause State"){
};

PauseState::~PauseState(){};

void PauseState::update(){
	if (ofGetKeyPressed('r')) {
		game->setTimerPaused(false);
		next();
	}
};

void PauseState::draw(){
	verdana.load("Arial Unicode.ttf", 13, true, true);
	ofBackground(ofColor::lightGoldenRodYellow);

	ofSetColor(ofColor::darkBlue);
	ofDrawRectangle(ofGetWidth() / 2 - 250, 250, 525, 190);
	ofSetColor(ofColor::white);
	/*
	ofDrawBitmapString(name, ofGetWidth() / 2 - 40, 270);
	ofDrawBitmapString("Press 'r' to resume", ofGetWidth() / 2 - 70, 350);
	*/
	verdana.drawString(name, ofGetWidth() / 2 - 40, 270);
	verdana.drawString("Press 'r' to resume", ofGetWidth() / 2 - 70, 350);
	/* verdana.drawString("Click on any key to back to the game ", ofGetWidth() / 2 - 100, 300); */
};

void PauseState::next(){
    game->setState(new PlayState(game));
};
