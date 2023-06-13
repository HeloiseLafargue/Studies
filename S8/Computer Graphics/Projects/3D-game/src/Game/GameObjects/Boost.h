#pragma once
#ifndef Boost_h
#define Boost_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class Boost : public GameObject {

public:
	Boost(Game *game, glm::vec3 pos, glm::vec3 dim);
	~Boost();

	void update() override;
	void draw() override;
	void receiveCarCollision(Player *car) override;

	ofPlanePrimitive plano;

	ofxAssimpModelLoader model;

	ofTexture texture;

};

#endif /* Boost_h */
