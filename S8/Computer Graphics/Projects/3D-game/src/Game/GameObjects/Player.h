#ifndef Player_h
#define Player_h

#include "ofMain.h"
#include "GameObject.h"

#include "ofxAssimpModelLoader.h"

class Game;

// TODO add speed settings
#define MAX_SPEED 100

class Player : public GameObject{
    
    ofLight faro;
    float speed;
    bool bLight;
    glm::vec3 prevPos;
    int coins;
    int laps;
    int cooldown;
    int updateValue;
    bool tunel;
    
public:
    
    Player(Game *game);
    ~Player();
    
    void init();
    void update() override;
    void draw() override;
    void drawDebug() override;
    void checkCollisions() override;
    float getSpeed();
    
    void steerLeft();
    void steerRight();
    void accelerate();
    void toggleLight();
    void shoot();
    void brake();
	void slow();
	void drift();
    void stop();
	void fall();
	void boost();
    void addCoins(int n = 1);
    void zeroCoins();
    int getCoins();
    
    ofxAssimpModelLoader model;
    
    void noCheat();
    int getLaps();
    void setLaps(int n);
    void goTunel(bool go);
};

#endif 
