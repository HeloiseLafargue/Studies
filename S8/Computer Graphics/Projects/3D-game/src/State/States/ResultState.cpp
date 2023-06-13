#include "Game.h"
#include "Player.h"
#include "ResultState.h"
#include "MenuState.h"

 
ResultState::ResultState(Game *game): State(game, "Result State"){
};

ResultState::~ResultState(){};

void ResultState::update(){
	if (ofGetKeyPressed('r')) {
		next();
	}
};

void ResultState::draw(){
	verdana.load("Arial Unicode.ttf", 13, true, true);
	ofBackground(ofColor::lightGoldenRodYellow);

	ofSetColor(ofColor::darkBlue);
	ofDrawRectangle(ofGetWidth() / 2 - 250, 250, 525, 190);
	ofSetColor(ofColor::white);

	verdana.drawString(name, ofGetWidth() / 2 - 40, 270);
	verdana.drawString("End of the game", ofGetWidth() / 2 - 50, 300);
	verdana.drawString("Score", ofGetWidth() / 2 - 5, 330);
	verdana.drawString(ofToString(game->getPlayer()->getCoins() + game->getEndTime() * 0.5, 2), ofGetWidth() / 2 - 10, 350);
	verdana.drawString("Press 'r' to try again Drift Fury", ofGetWidth() / 2 - 110, 390);
	/*
	ofDrawBitmapString(name, ofGetWidth() / 2 - 40, 270);
	ofDrawBitmapString("End of the game", ofGetWidth() / 2 - 50, 300);
	ofDrawBitmapString("Score", ofGetWidth() / 2 - 5, 330);
	//verdana.drawString("Score", ofGetWidth() / 2 - 200, 330);
	ofDrawBitmapString(ofToString(game->getPlayer()->getCoins() + game->getEndTime() * 0.5, 2), ofGetWidth() / 2 - 10, 350);
	ofDrawBitmapString("Press 'r' to try again Drift Fury", ofGetWidth() / 2 - 110, 390);
	*/

};

void ResultState::next(){
    game->setState(new MenuState(game));
};

void ResultState::getScore(Game* game) {
	score = game->getPlayer()->getCoins() + game->getEndTime() * 0.5;
}
