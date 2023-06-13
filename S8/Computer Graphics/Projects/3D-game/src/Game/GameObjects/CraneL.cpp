#include "CraneL.h"
#include "Player.h"
#include "Pedestrian.h"
#include "Game.h"

CraneL::CraneL(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    material.setEmissiveColor(ofColor::darkorange);
    collider->move(800, 0, 0);
    speed = 1;
}
CraneL::~CraneL(){}

void CraneL::draw(){
    
    material.begin();
    {
        collider->draw();
    }
    material.end();
    
}

void CraneL::update(){
    transform.rotateDeg(0 + speed, glm::vec3(0,1,0));
}



void  CraneL::receiveCarCollision(Player *car){
    car->stop();
	game->doObstacle();
}

void CraneL::receivePedestrianCollision(Pedestrian *pedestrian){
   pedestrian->turn();
}


