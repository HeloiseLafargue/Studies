#include "ColliderLaps.h"
#include "Player.h"
#include "Game.h"


ColliderLaps::ColliderLaps(Game *game, glm::vec3 pos, glm::vec3 dim) :
	GameObject(game, pos, dim) {
	
	plano.set(1500, 1500);
	plano.rotateDeg(-90, 1, 0, 0);
	plano.move(500, -49, -1700);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();
}
ColliderLaps::~ColliderLaps() {

}


void ColliderLaps::update() {
	model.update();
}

void ColliderLaps::draw() {
}

void ColliderLaps::receiveCarCollision(Player *car) {
	car->goTunel(true);
}

