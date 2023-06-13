#include "Barrera.h"
#include "Player.h"
#include "Road.h"
#include "Game.h"


Barrera::Barrera(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
    material.setEmissiveColor(ofColor::darkorange);
    
    speed = 2;
    bTurned = false;
}
Barrera::~Barrera(){}

void Barrera::update(){
    transform.move(transform.getYAxis() * -speed);
    bTurned = false;
}

void Barrera::draw(){
    material.begin();
    {
        collider->draw();
    }
    material.end();

}



void  Barrera::receiveCarCollision(Player *car){
    car->stop();
	game->doObstacle();
}

void  Barrera::receiveBulletCollision(GameObject *bullet){
    bullet->kill();
};

void Barrera::turn(){
    if(!bTurned){
        speed = -1*speed;
        bTurned = true;
    }
}

void Barrera::checkCollisions(){
    vector<GameObject*> collisions = game->getCollisions(this);
    for(auto c: collisions){
        c->receiveBarreraCollision(this);
    }
}



