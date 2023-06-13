#ifndef State_h
#define State_h

#include "ofMain.h"
class Game;

class State{

protected:
    string name;
    Game *game;
public:
    State(Game *game, string name): game(game), name(name){
        ofLogNotice() << "Creating state: " << name;
    };
    virtual ~State(){
        ofLogNotice() << "Destroying state: " << name;
    };
    
    virtual void update()=0;
    virtual void draw()=0;
    virtual void next()=0;
    virtual void keyPressed(int key){};
    virtual void keyReleased(int key){};
};

#endif
