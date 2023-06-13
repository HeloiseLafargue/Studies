#include "Hook.h"
#include "Player.h"
#include "Pedestrian.h"
#include "Game.h"

Hook::Hook(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    model.loadModel("hook.obj");
    collider->move(800, 0, 0);
    model.setRotation(0, 90, 1, 0, 0);
    model.setPosition(800, -100, 0);
    model.setScale(0.4, 0.4, 0.4);
    
    speedC = 1;
    speedL = 5;
}
Hook::~Hook(){}

void Hook::draw(){
    transform.transformGL();
    model.drawFaces();
    
  //  ofDrawAxis(200);
    transform.restoreTransformGL();
    
}

void Hook::update(){
    model.update();
    transform.rotateDeg(0 + speedC, glm::vec3(0,1,0));
    transform.move(transform.getYAxis() * -speedL);
    bTurned = false;
}



void  Hook::receiveCarCollision(Player *car){
    car->stop();
    car->zeroCoins();
}

void Hook::receivePedestrianCollision(Pedestrian *pedestrian){
   pedestrian->turn();
}


void Hook::turn(){
    if(!bTurned){
        speedL = -1*speedL;
        bTurned = true;
    }
}

void Hook::checkCollisions(){
    vector<GameObject*> collisions = game->getCollisions(this);
    for(auto c: collisions){
        c->receiveHookCollision(this);
        
    }
}

