#include "Dirt.h"
#include "Player.h"
#include "Game.h"


Dirt::Dirt(Game *game, glm::vec3 pos, glm::vec3 dim) :
	GameObject(game, pos, dim) {
	
	plano.set(1000, 1200);
	plano.rotateDeg(-90, 1, 0, 0);
	plano.move(2000, -49, 300);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();
	ofImage img;
	img.load("mud.jpg");

	texture = img.getTexture();



}
Dirt::~Dirt() {

}


void Dirt::update() {
	model.update();
}

void Dirt::draw() {

	texture.bind();
	plano.draw();
	texture.unbind();

}

void Dirt::receiveCarCollision(Player *car) {
	car->slow();
}

