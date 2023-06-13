

#include "Pedestrian.h"
#include "Player.h"
#include "Game.h"

Pedestrian::Pedestrian(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    model.loadModel("astroBoy_walk.dae");
    
    collider->move(0, dim.y/2 - 25, 0);
    model.setRotation(0, 180, 1, 0, 0);
    model.setPosition(0, -25, 0);
    model.setScale(0.25, 0.25, 0.25);
    model.setLoopStateForAllAnimations(OF_LOOP_NORMAL);
    model.playAllAnimations();
    
    transform.rotateDeg(90, 0, 1, 0);
    speed = 6;
    bTurned = false;
}
Pedestrian::~Pedestrian(){
    
}

void Pedestrian::update() {
    model.update();
    transform.move(transform.getZAxis() * -speed);
    bTurned = false;

};
void Pedestrian::draw(){
    transform.transformGL();
    model.drawFaces();
    
  //  ofDrawAxis(200);
    transform.restoreTransformGL();

    //collider->drawWireframe();
    
};
void Pedestrian::receiveCarCollision(Player *car) {
    kill();
    game->doScream();
};

void Pedestrian::receiveBulletCollision(GameObject *bullet) {
    
    bullet->kill();
    kill();
    game->getPlayer()->addCoins(1000);
 
};

void Pedestrian::turn(){
    if(!bTurned){
        transform.rotateDeg(180, 0, 1, 0);
        transform.move(transform.getZAxis() * -speed);
        bTurned = true;
    }
}


void Pedestrian::checkCollisions(){
    vector<GameObject*> collisions = game->getCollisions(this);
    for(auto c: collisions){
        c->receivePedestrianCollision(this);
    }
}
