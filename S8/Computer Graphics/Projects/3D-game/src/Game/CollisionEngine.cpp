#include "CollisionEngine.h"
#include "GameObject.h"
#include "Game.h"
#define BT_USE_DOUBLE_PRECISION

CollisionEngine::CollisionEngine(vector <GameObject*> &colliders):colliders(colliders){
    world.setup();
    world.setGravity( ofVec3f(0, 0, 0) );
    world.disableGrabbing();
    world.disableCollisionEvents();
    
};

CollisionEngine::~CollisionEngine(){
    world.destroy();
};


void CollisionEngine::add(GameObject *g){
    ofxBulletBox* box = new ofxBulletBox();
    BoxCollider *b = g->getCollider();
    box->init(ofBtGetBoxCollisionShape(b->getWidth(), b->getHeight(),  b->getDepth()));
    box->create(world.world, b->getGlobalPosition());
    box->setActivationState( DISABLE_DEACTIVATION );
    box->add();
    box->enableKinematic();
    if(!g->isFixed)
        box->activate();
    b->collisionObject = box;
   
    updateObject(g);
    
};

void CollisionEngine::updateObject(GameObject *g){
    BoxCollider *b = g->getCollider();
    btTransform transform;
    b->collisionObject->getRigidBody()->getMotionState()->getWorldTransform(transform);
    transform.setFromOpenGLMatrix(glm::value_ptr(b->getGlobalTransformMatrix()));
    b->collisionObject->getRigidBody()->getMotionState()->setWorldTransform(transform);
}

void CollisionEngine::update(){
    for(auto g: colliders){
        if(g->isFixed) continue;
        updateObject(g);
    }
    world.update(0.1, 1);
};

vector<GameObject *> CollisionEngine::getCollisions(GameObject *g){
    struct    MyContactResultCallback : public btCollisionWorld::ContactResultCallback
        {
            bool bCollison;
            MyContactResultCallback(){
                bCollison = false;
            }
            virtual    btScalar    addSingleResult(btManifoldPoint& cp,    const btCollisionObjectWrapper* colObj0Wrap,int partId0,int index0,const btCollisionObjectWrapper* colObj1Wrap,int partId1,int index1)
            {
                bCollison = true;
                return 0;
            }
        };

    vector<GameObject *> collisions;
    auto cw = world.getWorld()->getCollisionWorld();
    
    for(auto other: colliders){
        if(g != other){
            MyContactResultCallback resultCallback;
            
            cw->contactPairTest(g->getCollider()->collisionObject->getCollisionObject(),
                                other->getCollider()->collisionObject->getCollisionObject(),
                                resultCallback);
            if(resultCallback.bCollison){
                collisions.push_back(other);
            }
        }
            
    }
    return collisions;
};

void CollisionEngine::remove(GameObject *g){
    g->getCollider()->collisionObject->remove();

};


