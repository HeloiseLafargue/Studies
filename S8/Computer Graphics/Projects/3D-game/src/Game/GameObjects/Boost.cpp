#include "Boost.h"
#include "Player.h"
#include "Game.h"


Boost::Boost(Game *game, glm::vec3 pos, glm::vec3 dim) :
	GameObject(game, pos, dim) {

	model.loadModel("boost.obj");
	model.setRotation(0, 180, 1, 0, 0);
	model.setScale(0.25, 0.25, 0.25);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();



}
Boost::~Boost() {

}


void Boost::update() {
	model.update();
}

void Boost::draw() {

	transform.transformGL();
	model.drawFaces();
	transform.restoreTransformGL();

}

void Boost::receiveCarCollision(Player *car) {
	car->boost();
	game->doBoost();
	kill();
}

