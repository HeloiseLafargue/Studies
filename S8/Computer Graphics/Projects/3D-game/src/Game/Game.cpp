#include "Game.h"
#include "Player.h"

Game::Game(){
    // TODO create settings
    ROAD_WIDTH = 2000;
    ROAD_LENGTH = 10000;

    generator = new GameObjectGenerator(this);
    bDebug = false;
    scream.load("aaa.wav");
	bomb.load("bomb.flac");
	coin.load("coin.wav");
	obstacle.load("obstacle.flac");
	boost.load("boost2.flac");
	bgm.load("bgm.mp3");
	win.load("win.mp3");
	well.load("well.wav");
	bullet.load("bullet.mp3");
    verdana.load("Arial Unicode.ttf", 25, true, true);
}

Game::~Game(){
    ofLogNotice() << "Deleting game";
    delete gameObjects;
    delete generator;
    delete currentState();
}

void Game::init(){
    
    if(gameObjects != nullptr)
        delete gameObjects;
    
    
    gameObjects = new GameObjectContainer();
    
    player = new Player(this);
    player->init();

	bgm.setLoop(true);
	bgm.play();

    cam.setPosition(0, 150, -400);
    cam.setTarget(player->transform);
    cam.setParent(player->transform);
    //cam.disableMouseInput();
    cam.setFarClip(100000);
    
    gameObjects->add(player);
    generator->generateWorld();
    bPlayerFinish = false;
    initTime = ofGetElapsedTimef();

    pauseTimer = false;
    elapsedSeconds = 0.0;
}

void Game::beginPlay() {
    startTime = std::chrono::steady_clock::now();
}

void Game::update(){
    gameObjects->update();

    if (!pauseTimer) {
        std::chrono::steady_clock::time_point currentTime = std::chrono::steady_clock::now();
        std::chrono::duration<double> timeSpan = currentTime - startTime;
        elapsedSeconds = timeSpan.count();
    }
}

void Game::draw(){
    ofEnableLighting();
    ofEnableDepthTest();
    
    cam.begin();
    {
        if(bDebug) gameObjects->drawDebug();
        else gameObjects->draw();
    }
    cam.end();
    
    ofDisableLighting();
    ofDisableDepthTest();
}


Player * Game::getPlayer(){
    return player;
}

vector<GameObject *> Game::getCollisions(GameObject *gameObject){
    return gameObjects->getCollisions(gameObject);
}

void  Game::addGameObject(GameObject *gameobject){
    gameObjects->add(gameobject);
}

void Game::finishGame(float time){
    bPlayerFinish = true;
    endTime = time;
}

void Game::toggleDebug(){
    bDebug = !bDebug;
}

bool Game::isFinished(){
    return bPlayerFinish;
}

void Game::setFinished(bool v){
    bPlayerFinish = v;
}

double Game::getEllapsedTime(){
    return elapsedSeconds;
}

void Game::doScream(){
    scream.play();
}
void Game::doBomb() {
	bomb.play();
}
void Game::doCoin() {
	coin.play();
}
void Game::doBoost() {
	boost.play();
}

void Game::doObstacle() {
	obstacle.play();
}
void Game::doWin() {
	win.play();
}
void Game::doWell() {
	well.play();
}
void Game::doBullet() {
	bullet.play();
}
void Game::doBgm() {
	bgm.play();
}


void Game::setTimerPaused(bool paused) {
    pauseTimer = paused;
    if (paused) {
        // Stores the time elapsed before the timer is paused
        std::chrono::steady_clock::time_point currentTime = std::chrono::steady_clock::now();
        std::chrono::duration<double> timeSpan = currentTime - startTime;
        elapsedSeconds = timeSpan.count();
    }
    else {
        // Resume the timer from the previous elapsed time
        auto newStartTime = std::chrono::steady_clock::now() - std::chrono::duration<double>(elapsedSeconds);
        startTime = std::chrono::time_point_cast<std::chrono::steady_clock::duration>(newStartTime);
    }
}

float Game::getEndTime() {
    return endTime;
}
