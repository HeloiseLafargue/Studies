

#ifndef Goal_h
#define Goal_h
#include "GameObject.h"

class Goal : public GameObject{
    
    
public:
    Goal(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Goal();
    
    void draw() override;
    void drawDebug() override;
    void receiveCarCollision(Player *car) override;
    
};

#endif /* Goal_h */
