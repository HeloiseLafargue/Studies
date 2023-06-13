#include "ofApp.h"

//--------------------------------------------------------------
void ofApp::setup(){
	img.load("SuperMario.png");
	gui.setup();
	tamx = img.getWidth()/8;
	tamy = img.getHeight()/6;
	x = 0;
	y = 0;
	sprite_x = 0;
	sprite_y = 0;

}

//--------------------------------------------------------------
void ofApp::update(){
}

//--------------------------------------------------------------
void ofApp::draw(){
	if (ofGetKeyPressed('v')) {
		v = -1;
		sprite_x = (sprite_x + tamx) % (3 * tamx) ;
		sprite_y = 0;
		y = 0;
	} 
	else if (ofGetKeyPressed('b')) {
		v = 1;
		sprite_x = (sprite_x + tamx) % (3 * tamx);
		sprite_y = 0;
		y = 0;
	}
	else if (ofGetKeyPressed(' ')) {
		sprite_x = 5 * tamx;
		sprite_y = 0;
		y = -4;
	}
	else if (ofGetKeyPressed('n')) {
		sprite_x = 7 * tamx;
		sprite_y = 6 * tamy;
		y = 0;
	}

	x += v;
	ofTranslate(0, 200);
	ofScale(6, 6);
	ofTranslate(x, 0);
	
	if (v > 0) {
		ofTranslate(tamx, 0);
		ofScale(-1, 1);
		img.drawSubsection(0, y, tamx, tamy, sprite_x, sprite_y);
	}
	else {
		img.drawSubsection(0, y, tamx, tamy, sprite_x, sprite_y);
	}
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
