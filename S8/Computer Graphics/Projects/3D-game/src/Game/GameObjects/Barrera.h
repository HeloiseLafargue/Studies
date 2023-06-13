#ifndef Barrera_h
#define Barrera_h

#include "GameObject.h"

class Pedestrian;

class Barrera : public GameObject{
    float speed = -5;
    bool bTurned;
    
public:
    Barrera(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Barrera();
    
    float pos = pos;
    
    void draw() override;
    void update() override;
    void receiveCarCollision(Player *car) override;
    void receiveBulletCollision(GameObject *bullet) override;
    void checkCollisions() override;
    void turn();
    
};

#endif

