
#include "Goal.h"
#include "Player.h"
#include "Game.h"

Goal::Goal(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    material.setEmissiveColor(ofColor::green);
}
Goal::~Goal(){}

void Goal::draw(){
    
    material.begin();
    {
        transform.transformGL();
        
        ofDrawBox(-collider->getWidth()/2,
                  250, 0, 50, 500, 50);
        
        ofDrawBox(collider->getWidth()/2,
                  250, 0, 50, 500, 50);
        
        ofDrawBox(0,
                  500, 0, collider->getWidth(), 50, 50);
        
        transform.restoreTransformGL();
    }
    material.end();
}

void Goal::drawDebug(){
    collider->drawDebug();
}

void  Goal::receiveCarCollision(Player *car){
    car->noCheat();
    if (car->getLaps() >= 3) {
        float endTime = game->getEllapsedTime();
		game->doWin();
        game->finishGame(endTime);
    }    
}
