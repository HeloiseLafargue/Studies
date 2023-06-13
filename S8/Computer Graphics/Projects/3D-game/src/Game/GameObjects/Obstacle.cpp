#include "Obstacle.h"
#include "Player.h"
#include "Game.h"


Obstacle::Obstacle(Game *game, glm::vec3 pos, glm::vec3 dim):
    GameObject(game, pos, dim){

    model.loadModel("cone.obj");
	model.setRotation(0, 180, 1, 0, 0);
	model.setScale(0.25, 0.25, 0.25);
	model.setPosition(0, -50, 0);

	ofEnableNormalizedTexCoords();
	ofDisableArbTex();
        
 
    
}
Obstacle::~Obstacle(){
    
}


void Obstacle::update(){
    model.update();
}

void Obstacle::draw(){
    
	transform.transformGL();
	model.drawFaces();
	transform.restoreTransformGL();

}

void Obstacle::receiveCarCollision(Player *car){
    car->brake();
	game->doObstacle();
    kill();
}

void Obstacle::receiveBulletCollision(GameObject *bullet){
    bullet->kill();
    kill();
}

void Obstacle::setColor(ofColor color){
    material.setEmissiveColor(color);
}

