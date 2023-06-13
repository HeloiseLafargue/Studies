

#include "Road.h"
#include "Game.h"
#include "Barrera.h"

Road::Road(Game *game, glm::vec3 pos, glm::vec3 dim): GameObject(game, pos){
    material.setDiffuseColor(ofColor::darkGrey);
    
    plane.setParent(transform);
    transform.rotateDeg(-90, 1, 0, 0);
    plane.set(dim.x, dim.z);
    
	light.setPosition(500, 200, -1500);
	light.setDiffuseColor(ofColor::white);

	light2.setPosition(0, 200, 4000);
	light2.setDiffuseColor(ofColor::white);

	light3.setPosition(2500, 200, 200);
	light3.setDiffuseColor(ofColor::white);

	light4.setPosition(2500, 200, 5000);
	light4.setDiffuseColor(ofColor::white);

	light5.setPosition(1250, 200, 10000);
	light5.setDiffuseColor(ofColor::white);
}
Road::~Road(){}

void Road::draw(){
    /*
    light.enable();
	light2.enable();
	light3.enable();
	light4.enable();
	light5.enable();
	*/

    material.begin();
    {
        plane.draw();
    }
    material.end();
}


void Road::drawDebug(){
    plane.drawWireframe();
}

 void Road::receiveBarreraCollision(Barrera* barrera){
    barrera->turn();
 }
 
void Road::setColor(ofColor color){
    material.setDiffuseColor(ofColor::darkGrey);
}

void Road::lightEnable1() {
	light.enable();
}
void Road::lightDisable1() {
	light.disable();
}
void Road::lightEnable2() {
	light2.enable();
}
void Road::lightDisable2() {
	light2.disable();
}
void Road::lightEnable3() {
	light3.enable();
}
void Road::lightDisable3() {
	light3.disable();
}
void Road::lightEnable4() {
	light4.enable();
}
void Road::lightDisable4() {
	light4.disable();
}
void Road::lightEnable5() {
	light5.enable();
}
void Road::lightDisable5() {
	light5.disable();
}

