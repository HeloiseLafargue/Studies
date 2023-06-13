

#ifndef iColllide_h
#define iColllide_h

#include "ofMain.h"
#include "BoxCollider.h"

class Player;
class Pedestrian;
class Barrera;
class Hook;
class Bomb;

class iCollide{
    
    protected:
        BoxCollider *collider;

    public:
    
    virtual void receiveCarCollision(Player *car) {};
    virtual void receiveBulletCollision(GameObject *bullet) {};
    virtual void receivePedestrianCollision(Pedestrian *pedestrian) {};
    virtual void receiveBarreraCollision(Barrera* barrera) {};
    virtual void receiveHookCollision(Hook* hook) {};
    virtual void receiveBombCollision(Bomb* bomb) {};
        
    BoxCollider* getCollider(){
        return collider;
    }
        
    bool collide(iCollide *other){
        return collider->collide(other->getCollider());
    }
};

#endif /* iColllide_h */
