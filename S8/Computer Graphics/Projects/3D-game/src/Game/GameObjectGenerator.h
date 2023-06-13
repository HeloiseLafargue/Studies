
#ifndef GameObjectGenerator_h
#define GameObjectGenerator_h

#include "ofMain.h"
class Game;

class GameObjectGenerator{
    Game *game;
    
public:
    GameObjectGenerator(Game *game);
    
    void generateWorld();
};


#endif
