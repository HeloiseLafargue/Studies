#include "ofApp.h"

//--------------------------------------------------------------
void ofApp::setup(){
	ofSetWindowTitle("Juego de reflejos");
	jugador1 = false;
	jugador2 = false;
	ronda = 0;
	tiempo = 0;
}

//--------------------------------------------------------------
void ofApp::update(){
	if (ronda < 10) {
		if (ofGetElapsedTimef() - tiempo > ofRandom(3, 5)) {
			figura1 = ofRandom(2);
			figura2 = ofRandom(2);
			color1 = ofRandom(2);
			color2 = ofRandom(2);

			tiempo = ofGetElapsedTimef();
			ronda++;
		}		
	}
	ofSetColor(ofColor::red);
	ofDrawBitmapString("End of the game (10 iterations)", ofGetWidth() / 2, ofGetHeight() / 2);

}

//--------------------------------------------------------------
void ofApp::draw(){

	if (ronda < 10) {

		//if (gameStarted) {

			if (color1 == 0) {
				ofSetColor(ofColor::blue);
				if (figura1 == 0) {
					ofDrawCircle(ofGetWidth() / 4, ofGetHeight() / 2, 50);
				}
				else {
					ofDrawRectangle(ofGetWidth() / 4, ofGetHeight() / 2, 50, 50);
				}
			}
			else {
				ofSetColor(ofColor::red);
				if (figura1 == 0) {
					ofDrawCircle(ofGetWidth() / 4, ofGetHeight() / 2, 50);
				}
				else {
					ofDrawRectangle(ofGetWidth() / 4, ofGetHeight() / 2, 50, 50);
				}
			}

			if (color2 == 0) {
				ofSetColor(ofColor::blue);
				if (figura2 == 0) {
					ofDrawCircle(ofGetWidth() * 3 / 4, ofGetHeight() / 2, 50);
				}
				else {
					ofDrawRectangle(ofGetWidth() * 3 / 4, ofGetHeight() / 2, 50, 50);
				}
			}
			else {
				ofSetColor(ofColor::red);
				if (figura2 == 0) {
					ofDrawCircle(ofGetWidth() * 3 / 4, ofGetHeight() / 2, 50);
				}
				else {
					ofDrawRectangle(ofGetWidth() * 3 / 4, ofGetHeight() / 2, 50, 50);
				}
			}
		//}
	}
}

//--------------------------------------------------------------
void ofApp::keyPressed(int key){
	if (!gameStarted) {
		gameStarted = true;
		tiempo = ofGetElapsedTimef();
	}
	else {
		if (figura1 == figura2 || color1 == color2) {
			if ((key == 'q') && !jugador2) {
				jugador1 = true;
				ofSetColor(0, 255, 0);
				ofDrawBitmapString("Jugador 1 Gana", ofGetWidth() / 2, ofGetHeight() / 2);
			}
			if ((key == 'o') && !jugador1) {
				jugador2 = true;
				ofSetColor(0, 255, 0);
				ofDrawBitmapString("Jugador 2 Gana", ofGetWidth() / 2, ofGetHeight() / 2);
			}
			else {
				ofDrawBitmapString("Press any key to start the game", ofGetWindowWidth() / 2, ofGetWindowHeight() / 2);
			}
		}
		else if (!(figura1 == figura2) && !(color1 == color2)) {
			if ((key == 'w') && !jugador2) {
				jugador1 = false;
				ofSetColor(255, 0, 0);
				ofDrawBitmapString("Jugador 1 Pierde", ofGetWidth() / 2, ofGetHeight() / 2);
			} else if ((key == 'w') && !jugador2) {
				jugador2 = false;
				ofSetColor(255, 0, 0);
				ofDrawBitmapString("Jugador 2 Pierde", ofGetWidth() / 2, ofGetHeight() / 2);
			}
			else {
				ofDrawBitmapString("Press any key to start the game", ofGetWindowWidth() / 2, ofGetWindowHeight() / 2);
			}
		}
	}
}

//--------------------------------------------------------------
void ofApp::keyReleased(int key){

}

//--------------------------------------------------------------
void ofApp::mouseMoved(int x, int y ){

}

//--------------------------------------------------------------
void ofApp::mouseDragged(int x, int y, int button){

}

//--------------------------------------------------------------
void ofApp::mousePressed(int x, int y, int button){

}

//--------------------------------------------------------------
void ofApp::mouseReleased(int x, int y, int button){

}

//--------------------------------------------------------------
void ofApp::mouseEntered(int x, int y){

}

//--------------------------------------------------------------
void ofApp::mouseExited(int x, int y){

}

//--------------------------------------------------------------
void ofApp::windowResized(int w, int h){

}

//--------------------------------------------------------------
void ofApp::gotMessage(ofMessage msg){

}

//--------------------------------------------------------------
void ofApp::dragEvent(ofDragInfo dragInfo){ 

}
