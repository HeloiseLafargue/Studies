#ifndef CollisionEngine_h
#define CollisionEngine_h

#include "ofMain.h"
#include "ofxBullet.h"
class Game;
class GameObject;

class CollisionEngine{    
    ofxBulletWorldRigid            world;
    vector <GameObject*> &colliders;
    GameObject *targetObject;
    
    void updateObject(GameObject*);
public:
    CollisionEngine(vector <GameObject*> &colliders);
    ~CollisionEngine();
    void update();
    void add(GameObject *g);
    void remove(GameObject *g);
    
    vector<GameObject *> getCollisions(GameObject *g);
};

#endif /* CollisionEngine_h */
