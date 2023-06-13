#pragma once
#ifndef Dirt_h
#define Dirt_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class Dirt : public GameObject {

public:
	Dirt(Game *game, glm::vec3 pos, glm::vec3 dim);
	~Dirt();

	void update() override;
	void draw() override;
	void receiveCarCollision(Player *car) override;

	ofPlanePrimitive plano;

	ofxAssimpModelLoader model;

	ofTexture texture;

};

#endif /* Dirt_h */
