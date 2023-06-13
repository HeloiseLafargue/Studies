#ifndef Hook_h
#define Hook_h

#include "GameObject.h"
#include "Barrera.h"

#include "ofxAssimpModelLoader.h"

class Pedestrian;

class Hook : public GameObject{
    float speedC = -5;
    float speedL = -5;
    bool bTurned;
    
public:
    Hook(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Hook();
    
    void draw() override;
    void update() override;
    void receiveCarCollision(Player *car) override;
    void receivePedestrianCollision(Pedestrian *pedestrian) override;
    void checkCollisions() override;
    void turn();
    ofxAssimpModelLoader model;
};

#endif

