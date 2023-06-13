#include "CraneW.h"
#include "Player.h"
#include "Pedestrian.h"
#include "Game.h"

CraneW::CraneW(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    material.setEmissiveColor(ofColor::darkorange);
    collider->move(400, 0, 0);
    speed = 1;
}
CraneW::~CraneW(){}

void CraneW::draw(){
    
    material.begin();
    {
        collider->draw();
    }
    material.end();
    
}

void CraneW::update(){
    transform.rotateDeg(0 + speed, glm::vec3(0,1,0));
}

void  CraneW::receiveCarCollision(Player *car){
    car->stop();
	game->doObstacle();
}

void CraneW::receivePedestrianCollision(Pedestrian *pedestrian){
   pedestrian->turn();
}

void CraneW::receiveHookCollision(Hook* hook){
   hook->turn();
}


