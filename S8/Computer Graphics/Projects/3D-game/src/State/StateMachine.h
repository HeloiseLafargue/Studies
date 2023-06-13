
#ifndef StateMachine_h
#define StateMachine_h

#include "State.h"

class StateMachine{
  
    State *state = nullptr;
    
public:
    
    void setState(State *s){
        auto prevState = state;
        state = s;
        if (prevState != nullptr)
            delete prevState;
    }
    State *currentState(){
        return state;
    }
};

#endif
