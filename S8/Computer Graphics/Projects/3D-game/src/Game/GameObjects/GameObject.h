#ifndef GameObject_h
#define GameObject_h

#include "ofMain.h"
#include "iCollide.h"
#include "BoxCollider.h"

class Game;
class Player;

class GameObject: public iCollide{
    
protected:
    ofMaterial material;
    bool bAlive;
    Game *game;
    void init(glm::vec3 pos);
    
public:
    ofNode transform;
    GameObject(Game *game, glm::vec3 pos);
    GameObject(Game *game, glm::vec3 pos,  glm::vec3 dim);
    virtual ~GameObject();
    
    virtual void draw() = 0;
    virtual void drawDebug();
    virtual void update() {};
    virtual void checkCollisions(){};
    bool isAlive();
    void kill(){bAlive = false;};

    bool isFixed = false;
};

#endif 
