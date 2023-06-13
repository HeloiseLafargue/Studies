
#ifndef BoxCollider_h
#define BoxCollider_h

#include "ofMain.h"

class GameObject;
class ofxBulletRigidBody;

class BoxCollider : public ofBoxPrimitive{
    GameObject *gameObject;
    bool bColliding;
public:
    ofxBulletRigidBody *collisionObject;
    BoxCollider(GameObject *gameObject);
    ~BoxCollider();
    bool collide(BoxCollider *other);
    void drawDebug();
    void setColliding(bool v){bColliding = true;};
    
};


#endif /* BoxCollider_h */
