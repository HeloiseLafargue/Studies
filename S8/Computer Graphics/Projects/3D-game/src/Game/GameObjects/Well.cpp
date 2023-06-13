#include "Well.h"
#include "Player.h"
#include "Game.h"


Well::Well(Game *game, glm::vec3 pos, glm::vec3 dim) :
	GameObject(game, pos, dim) {

	plano.set(800, 800);
	plano.rotateDeg(-90, 1, 0, 0);
	plano.move(2200, -49, 9500);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();
	ofImage img;
	img.load("well.jpg");

	texture = img.getTexture();



}
Well::~Well() {

}


void Well::update() {
	model.update();
}

void Well::draw() {

	texture.bind();
	plano.draw();
	texture.unbind();

}

void Well::receiveCarCollision(Player *car) {
	car->fall();
	game->doWell();
	car->setLaps(0);
}

