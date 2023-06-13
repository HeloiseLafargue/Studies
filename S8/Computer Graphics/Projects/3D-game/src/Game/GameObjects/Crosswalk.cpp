#include "Crosswalk.h"
#include "Player.h"
#include "Pedestrian.h"
#include "Barrera.h"

Crosswalk::Crosswalk(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    
	plano.set(2000, 200);
	plano.rotateDeg(-90, 1, 0, 0);
	plano.move(0, -49, 500);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();
	ofImage img;
	img.load("crosswalk.jpg");

	texture = img.getTexture();
}
Crosswalk::~Crosswalk(){}

void Crosswalk::draw(){
    
	texture.bind();
	plano.draw();
	texture.unbind();
}


