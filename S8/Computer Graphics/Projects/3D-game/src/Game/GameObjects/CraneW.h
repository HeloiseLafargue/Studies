#ifndef CraneW_h
#define CraneW_h

#include "GameObject.h"
#include "Hook.h"

class Pedestrian;

class CraneW : public GameObject{
    float speed = -5;
    
public:
    CraneW(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~CraneW();
    
    void draw() override;
    void update() override;
    void receiveCarCollision(Player *car) override;
    void receivePedestrianCollision(Pedestrian *pedestrian) override;
    void receiveHookCollision(Hook* hook) override;
    
};

#endif

