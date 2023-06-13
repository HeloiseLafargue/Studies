#ifndef PauseState_h
#define PauseState_h

#include "State.h"
class Game;

class PauseState: public State{
public:
    PauseState(Game *game);
    
    ~PauseState();
    
    void update();
    void draw();
    void next();

    ofTrueTypeFont verdana;
        
};

#endif /* PauseState_h */
