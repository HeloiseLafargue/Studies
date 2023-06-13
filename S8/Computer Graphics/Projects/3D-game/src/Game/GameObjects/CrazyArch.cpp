#include "CrazyArch.h"
#include "Player.h"
#include "Game.h"


CrazyArch::CrazyArch(Game *game, glm::vec3 pos, glm::vec3 dim) :
	GameObject(game, pos, dim) {

	plano.set(600, 400);
	plano.rotateDeg(180, 0, 1, 0);
	plano.move(300, 400, 8500);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();

	
	
	image.load("zero.png");
	image1.load("four.png");
	image2.load("twenty.png");
	image3.load("hundred.jpg");
	death.load("death.jpg");

	texture = image.getTexture();

	updateValue = 0;
	archValue = 1;
	cooldown = 0;
	prize = 0;

	fbo.allocate(600, 600);
	texture = fbo.getTexture();



}
CrazyArch::~CrazyArch() {

}


void CrazyArch::update() {

	updateValue++;



	if (updateValue > 100) {
		updateValue = 0;
		cooldown = 0;

		archValue = rand() % 5;

		switch (archValue){
			case 0:
				texture = image.getTexture();
				prize = 0;
				break;
			case 1:
				texture = image1.getTexture();
				prize = 4;
				break;
			case 2:
				texture = image2.getTexture();
				prize = 20;
				break;
			case 3:
				texture = image3.getTexture();
				prize = 100;
				break;
			case 4:
				texture = death.getTexture();
				prize = -1;
				break;
			default:
				texture = image.getTexture();
				break;
		}			
	}
	fbo.begin();
	ofBackground(255);
	ofSetColor(255, 0, 0);
	ofSetCircleResolution(100);
	ofDrawCircle(600, 600, ofGetFrameNum() % 600);
	fbo.end();
}

void CrazyArch::draw() {

	texture.bind();
	plano.draw();
	texture.unbind();

}

void CrazyArch::receiveCarCollision(Player *car) {
	if (cooldown == 0) {
		if (prize != -1) {
			car->addCoins(prize);
			game->doCoin();
		}
		else {
			car->zeroCoins();
			game->doObstacle();
		}
	
		cooldown = 1;

	}
}

