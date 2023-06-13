#include "Player.hpp"


//--------------------------------------------------------------
void Player::setup(){



}

//--------------------------------------------------------------
void Player::update(){

}

//--------------------------------------------------------------
void Player::draw(){
	ofDrawCircle(150, position->y_coord, 0, 25);
}

//--------------------------------------------------------------
void Player::setId(int newId) {
	id = newId;
}

//--------------------------------------------------------------
int Player::getId() {
	return id;
}

//--------------------------------------------------------------
void Player::setPoints(int newPoints) {
	points = newPoints;
}

//--------------------------------------------------------------
int Player::getPoints() {
	return points;
}

//--------------------------------------------------------------
void Player::setRail(Rail* newRail) {
    position = newRail;
}

//--------------------------------------------------------------
Rail* Player::getRail() {
    return position;
}

//--------------------------------------------------------------
void Player::setWins(int newWins) {
	wins = newWins;
}

//--------------------------------------------------------------
int Player::getWins() {
	return wins;
}
