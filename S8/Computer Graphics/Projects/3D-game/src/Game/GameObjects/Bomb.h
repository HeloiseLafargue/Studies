#ifndef Bomb_h
#define Bomb_h

#include "GameObject.h"
#include "ofxAssimpModelLoader.h"


class Bomb : public GameObject{
    float speed = -5;
    
public:
    Bomb(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Bomb();
    
    void draw() override;
    void update() override;
    void checkCollisions() override;
    void receiveCarCollision(Player *car) override;

	ofxAssimpModelLoader model;
    
};

#endif

