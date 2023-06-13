#include "GameObjectContainer.h"
#include "GameObject.h"
#include "CollisionEngine.h"

GameObjectContainer::GameObjectContainer(){
    collisionEngine = new CollisionEngine(gameObjects);
}

void GameObjectContainer::add(GameObject* g){
    gameObjects.push_back(g);
    collisionEngine->add(g);
}

GameObjectContainer::~GameObjectContainer(){
    clear();
    delete collisionEngine;
}

void GameObjectContainer::update(){

    for(auto g: gameObjects){
        if(g->isAlive()){
            g->update();
        }
    }
    
    collisionEngine->update();
    
    for(auto g: gameObjects){
        if(g->isAlive()){
            g->checkCollisions();
        }
    }
    removeDead();
}

void GameObjectContainer::draw(){
    for(auto g: gameObjects){
        if(g->isAlive()){
            g->draw();
        }
    }
}
void GameObjectContainer::drawDebug(){
    for(auto g: gameObjects){
        g->drawDebug();
    }
}

void GameObjectContainer::removeDead(){
    vector<GameObject*> alive;
    for(auto g: gameObjects){
        if(g->isAlive())
            alive.push_back(g);
        else{
            collisionEngine->remove(g);
            delete g;
        }
    }
    gameObjects.clear();
    gameObjects = alive;
}

void GameObjectContainer::clear(){
    for(auto g: gameObjects){
        collisionEngine->remove(g);
        delete g;
    }
    gameObjects.clear();
}

vector<GameObject *> GameObjectContainer::getCollisions(GameObject *gameObject){
    return collisionEngine->getCollisions(gameObject);
}
