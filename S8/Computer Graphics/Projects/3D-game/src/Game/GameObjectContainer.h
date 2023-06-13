
#ifndef GameObjectContainer_h
#define GameObjectContainer_h

#include "ofMain.h"

class GameObject;
class CollisionEngine;

class GameObjectContainer{
    
    vector<GameObject *> gameObjects;
    CollisionEngine *collisionEngine;
public:
    GameObjectContainer();
    ~GameObjectContainer();
    
    void add(GameObject* g);
    void update();
    void draw();
    void drawDebug();
    void removeDead();
    void clear();
    vector<GameObject *> getCollisions(GameObject *gameObject);
};
#endif 
