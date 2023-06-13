#ifndef Bombardier_h
#define Bombardier_h

#include "GameObject.h"
#include "Bomb.h"
#include "ofxAssimpModelLoader.h"



class Bombardier : public GameObject{
    float speed = -5;
    glm::vec3 position;
    
public:
    Bombardier(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Bombardier();
    
    void draw() override;
    void update() override;

	ofxAssimpModelLoader model;
    
};

#endif

