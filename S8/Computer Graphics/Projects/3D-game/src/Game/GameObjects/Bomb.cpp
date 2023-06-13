#include "Bomb.h"
#include "Player.h"
#include "Game.h"
#include "Bombardier.h"

Bomb::Bomb(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos, dim){
	model.loadModel("bomb.obj");
	model.setRotation(0, 180, 1, 0, 0);
	model.setScale(0.25, 0.25, 0.25);
    speed = 2;
}
Bomb::~Bomb(){}

void Bomb::draw(){
    
	transform.transformGL();
	model.drawFaces();
	transform.restoreTransformGL();
    
}

void Bomb::update(){

	int rand_x = 0, rand_z = 0;
	model.update();
    transform.move(transform.getYAxis() * -speed);

	rand_x = rand() % 2;
	rand_z = rand() % 2;

	if (rand_x)
		rand_x = 1000;
	else
		rand_x = -1000;

	if (rand_z)
		rand_z = 1000;
	else
		rand_z = -1000;
    
	if (transform.getPosition().y <= -200) {
		
		transform.move(rand_x, 1200, rand_z);
	}
    
}

void  Bomb::receiveCarCollision(Player *car){
	game->doBomb();
	car->fall();
	kill();
	game->finishGame(100000);
}

void Bomb::checkCollisions(){
    vector<GameObject*> collisions = game->getCollisions(this);
    for(auto c: collisions){
        c->receiveBombCollision(this);
    }
}
