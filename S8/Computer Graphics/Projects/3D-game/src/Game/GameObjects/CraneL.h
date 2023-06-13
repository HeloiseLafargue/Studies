#ifndef CraneL_h
#define CraneL_h

#include "GameObject.h"
#include "Barrera.h"

class Pedestrian;

class CraneL : public GameObject{
    float speed = -5;
    
public:
    CraneL(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~CraneL();
    
    void draw() override;
    void update() override;
    void receiveCarCollision(Player *car) override;
    void receivePedestrianCollision(Pedestrian *pedestrian) override;
    
};

#endif

