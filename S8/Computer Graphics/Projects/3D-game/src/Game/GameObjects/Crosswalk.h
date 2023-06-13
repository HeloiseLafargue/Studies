#ifndef Crosswalk_h
#define Crosswalk_h

#include "GameObject.h"


class Crosswalk : public GameObject{
    
    
public:
    Crosswalk(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Crosswalk();
    
    void draw() override;

	ofPlanePrimitive plano;

	ofTexture texture;
};

#endif

