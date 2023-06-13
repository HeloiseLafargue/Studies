#include "Player.h"
#include "Game.h"
#include "Bullet.h"

Player::Player(Game *game):GameObject(game, glm::vec3(100)){
    
    material.setDiffuseColor(ofColor::blue);
    model.loadModel("car.obj");
    model.setRotation(0, 180, 1, 0, 0);
    //model.setRotation(0, 180, 0, 0, 1);
    model.setScale(0.5, 0.5, 0.5);
    //model.setPosition(0, 0, 0);
    
    faro.setParent(transform);
    faro.setDiffuseColor(ofColor::yellow);
    faro.setSpotlight();
    faro.move(0, 0, 50);
    faro.rotateDeg(-200, 1, 0, 0);
}

Player::~Player(){}

void Player::init(){
    transform.setPosition(0, 0, 0);
    speed = 0;
    bLight = false;
    
    coins = 10;
    laps = 0;
    cooldown = 0;
    updateValue = 0;
    tunel = false;
}

void Player::update(){
    model.update();
    prevPos = transform.getPosition();
    transform.move(transform.getZAxis() * speed);
    
    if(speed > MAX_SPEED) speed = MAX_SPEED;
    if(speed < 0) speed = 0;
    
    updateValue++;
    if (updateValue > 100) {
        updateValue = 0;
        cooldown = 0;
    }
}

void Player::draw(){
    transform.transformGL();
    model.drawFaces();
    
  //  ofDrawAxis(200);
    transform.restoreTransformGL();
    faro.draw();
    if(bLight)
        faro.enable();
    else
        faro.disable();
}

void Player::drawDebug(){
    collider->drawDebug();
    
    transform.transformGL();
    ofDrawAxis(100);
    transform.restoreTransformGL();
}

void Player::checkCollisions(){
    vector<GameObject*> collisions = game->getCollisions(this);
    for(auto c: collisions){
        c->receiveCarCollision(this);
    }
}

void Player::steerLeft(){
    transform.rotateDeg(1, 0, 2, 0);
}
void Player::steerRight(){
    transform.rotateDeg(-1, 0, 2, 0);
}
void Player::accelerate(){
    speed += 0.1;
}
void Player::brake(){
    speed -= 10;
}
void Player::slow() {
	if (speed > 2)
		speed -= 0.5;
	else
		speed = 2;
}
void Player::drift() {
	int crazy = rand() % 100;
	int lor = rand() % 100;

	/*No derrapa igual, siempre gira mas para la derecha*/
	if (crazy > 70) {
		if (lor > 70)
			transform.rotateDeg(1, 0, 2, 0);
		else
			transform.rotateDeg(-2, 0, 2, 0);
	}
	/* aumenta la velocidad porque derrapa (loses control) */
	speed += 0.1;
}

void Player::fall() {
	transform.setPosition(0, 0, 0);
	speed = 0;
	bLight = false;

	coins = 0;
}

void Player::boost() {
	speed += 10;
}

void Player::stop(){
    speed = 0;
    transform.setPosition(prevPos);
}

void Player::toggleLight(){
    bLight = !bLight;
}

float Player::getSpeed(){
    return speed;
}

void Player::addCoins(int n){
    coins += n;
}

void Player::zeroCoins(){
    coins = 0;
}

int Player::getCoins(){
    return coins;
}
void Player::shoot(){
    if (coins > 0){
        game->addGameObject(new Bullet(game, transform));
        coins--;
		game->doBullet();
    }
}
void Player::goTunel(bool go) {
    if (go) {
        tunel = true;
    }
}
void Player::noCheat() {
    if (tunel) {
        if (cooldown == 0) {
            setLaps(laps + 1);
            cooldown = 1;
            tunel = false;
        }
    }
}
int Player::getLaps() {
    return laps;
}
void Player::setLaps(int n) {
    laps = n;
}

