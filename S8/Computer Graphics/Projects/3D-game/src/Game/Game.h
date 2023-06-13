#ifndef Game_h
#define Game_h

#include "ofMain.h"
#include "StateMachine.h"
#include "GameObjectContainer.h"
#include "GameObjectGenerator.h"

#include <iostream>
#include <chrono>
#include <thread>

class Player;

class Game : public StateMachine{
    ofEasyCam cam;
    Player *player;
    GameObjectContainer *gameObjects = nullptr;
    GameObjectGenerator *generator  = nullptr;
    bool bDebug;
    bool bPlayerFinish;
    float initTime;
    float pauseTime;
    float endTime;
    
    ofSoundPlayer scream;
	ofSoundPlayer bomb;
	ofSoundPlayer boost;
	ofSoundPlayer coin;
	ofSoundPlayer obstacle;
	ofSoundPlayer well;
	ofSoundPlayer win;
	ofSoundPlayer bullet;
	ofSoundPlayer bgm;
    ofTrueTypeFont verdana;

private : 
    bool pauseTimer;
    std::chrono::steady_clock::time_point startTime;
    double elapsedSeconds;
    
public:
    int ROAD_LENGTH;
    int ROAD_WIDTH;
    
    Game();
    ~Game();
    void init();
    void update();
    void draw();
    void finishGame(float time);
    void toggleDebug();
    bool isFinished();
    void setFinished(bool v);
    
    Player *getPlayer();
    vector<GameObject *> getCollisions(GameObject *gameObject);
    void addGameObject(GameObject *gameobject);
    
    double getEllapsedTime();
    void doScream();
	void doBomb();
	void doBoost();
	void doCoin();
	void doObstacle();
	void doWell();
	void doWin();
	void doBullet();
	void doBgm();
    float getEndTime();
    void setTimerPaused(bool timePaused);
    void beginPlay();
    
};
#endif
