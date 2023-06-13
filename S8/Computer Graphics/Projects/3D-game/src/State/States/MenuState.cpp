#include "PlayState.h"
#include "MenuState.h"
#include "Game.h"

MenuState::MenuState(Game* game) : State(game, "Menu State") {
    game->init();
};

void MenuState::setup() {
    gui.setup();
    gui.add(dif.set("Difficulty", 1, 1, 6));
    gui.add(totalGames.set("Number of Games", 1, 1, 10));

    verdana.load("Arial Unicode.ttf", 13, true, true);
    logo.load("driftFury.jpg");
	
}
void MenuState::update(){
	setup();
    if (ofGetKeyPressed()) {
        game->beginPlay();
        next();
    }
	
};
    
void MenuState::draw(){
    
    gui.draw();
	


    ofBackground(ofColor::lightGoldenRodYellow);


	logo.draw(ofGetWidth() / 2 - 250, ofGetHeight() / 2 - 400, 500, 500);

    ofSetColor(ofColor::darkBlue);
    ofDrawRectangle(ofGetWidth() / 2 - 250, 300, 525, 190);
    //ofDrawBitmapString("DRIFT FURY", ofGetWidth() / 2 - 30, 200);
	
	
    ofSetColor(ofColor::white);
	verdana.drawString(name, ofGetWidth() / 2 - 40, 330);
	verdana.drawString("Press any key to Start", ofGetWidth() / 2 - 90, 360);
	verdana.drawString("Press 'p' to pause", ofGetWidth() / 2 - 70, 410);
	verdana.drawString("Press 'l' to light", ofGetWidth() / 2 - 70, 430);
	verdana.drawString("Press 'space' to shoot", ofGetWidth() / 2 - 80, 450);
	/*
    ofDrawBitmapString(name, ofGetWidth() / 2 - 40, 330);
    ofDrawBitmapString("Press any key to Start", ofGetWidth() / 2 - 90, 360);
    ofDrawBitmapString("Press 'p' to pause", ofGetWidth() / 2 - 70, 410);
    ofDrawBitmapString("Press 'l' to light", ofGetWidth() / 2 - 70, 430);
    ofDrawBitmapString("Press 'space' to shot", ofGetWidth() / 2 - 80, 450);
	*/
    //verdana.drawString("test", ofGetWidth() / 2 - 200, 330);
	
  
};

void MenuState::next(){
    game->setState(new PlayState(game));
};
