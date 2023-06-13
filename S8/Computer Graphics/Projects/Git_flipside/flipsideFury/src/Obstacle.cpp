//
//  Obstacle.cpp
//  flipsideFury
//
//  Created by Hanjie Zhu on 17/3/23.
//

#include "Obstacle.hpp"

void Obstacle::draw(){
	
	
	
    if (type == 0){
		ofPushMatrix();
        ofTranslate(x_coord, rail->y_coord - 25);
		ofSetColor(ofColor::yellow);
        ofDrawTriangle(0,50,25,0,50,50);
		ofPopMatrix();
    }else if (type == 1){
		ofPushMatrix();
        ofTranslate(x_coord, rail->y_coord - 25);
		ofSetColor(ofColor::orange);
        ofDrawRectangle(0, 0, 25, 50);
		ofPopMatrix();
    }else if (type == 2) {
		ofPushMatrix();
		ofTranslate(x_coord, rail->y_coord);
		ofSetColor(ofColor::green);
		ofDrawCircle(0, 0, 0, 25);
		ofPopMatrix();
	}
	
}

void Obstacle::setRail(Rail* newRail) {
    rail = newRail;
}

Rail* Obstacle::getRail() {
    return rail ;
}

void Obstacle::setType(int newtype) {
    type = newtype;
}

int Obstacle::getType() {
    return type;
}

void Obstacle::setXcoord(int newXcoord) {
    x_coord = newXcoord;
}

int Obstacle::getXcoord() {
    return x_coord;
}

void Obstacle::setSpeed(int newSpeed) {
    speed = newSpeed;
}

int Obstacle::getSpeed() {
    return speed;
}

