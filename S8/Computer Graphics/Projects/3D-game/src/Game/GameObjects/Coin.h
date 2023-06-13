#ifndef Coin_h
#define Coin_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class Coin : public GameObject{
    
public:
    Coin(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Coin();
    
    void update() override;
    void draw() override;
    void receiveCarCollision(Player *car) override;
    void receiveBulletCollision(GameObject *bullet) override;
  
    ofxAssimpModelLoader model;

};

#endif /* Coin_h */
