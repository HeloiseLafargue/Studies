#include "ofApp.h"

//--------------------------------------------------------------
void ofApp::setup(){
	img.load("background.png");
	gui.setup();
	ofBackground(ofColor::darkGray);
	state = arriba;
	waitingTime = 3;
}

//--------------------------------------------------------------
void ofApp::update(){
	if (ofGetElapsedTimef() > waitingTime) {
		if (state == arriba) {
			object.x = ofRandom(0, ofGetWidth());
			object.y = 0;
			object.randColor = ofRandom(0, 2);
			state = cae;
		}
		else if (state == cae) {
			object.x += 100;
			object.y -= 100;
		}
		else {
			ofSetColor(255);
			s = ofToString("Cuadarado Abajo");
			state = arriba;
		}
	}
}

//--------------------------------------------------------------
void ofApp::draw(){
	img.drawSubsection(0, 0, 500, 500, 50, 50);

	if (object.randColor == 0) { ofSetColor(ofColor::white); }
	else { ofSetColor(ofColor::red); }
	ofDrawRectangle(object.x, object.y, 80, 80);
	
	// Barco
	ofSetColor(ofColor::darkBlue);
	ofDrawRectangle(mouseX, ofGetHeight()*3/4, 200, 50);
}

//--------------------------------------------------------------
void ofApp::keyPressed(int key){

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
