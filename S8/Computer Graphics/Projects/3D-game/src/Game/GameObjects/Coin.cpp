#include "Coin.h"
#include "Player.h"
#include "Game.h"


Coin::Coin(Game *game, glm::vec3 pos, glm::vec3 dim):
    GameObject(game, pos, dim){
	model.loadModel("coin.obj");
	model.setRotation(0, 180, 1, 0, 0);
	model.setRotation(0, 90, 0, 1, 0);
	model.setPosition(0, 75, 0);
	model.setScale(0.15, 0.15, 0.15);
    model.enableColors();
        
 
}
Coin::~Coin(){
    
}


void Coin::update(){
    model.update();
}

void Coin::draw(){
	transform.transformGL();
	model.drawFaces();
	transform.restoreTransformGL();
}

void Coin::receiveCarCollision(Player *car){
    car->addCoins(5);
    kill();
	game->doCoin();
}

void Coin::receiveBulletCollision(GameObject *bullet){
    bullet->kill();
    kill();
    game->getPlayer()->addCoins(1000);
}
