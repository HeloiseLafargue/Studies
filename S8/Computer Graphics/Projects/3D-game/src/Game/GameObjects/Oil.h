#pragma once
#ifndef Oil_h
#define Oil_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class Oil : public GameObject {

public:
	Oil(Game *game, glm::vec3 pos, glm::vec3 dim);
	~Oil();

	void update() override;
	void draw() override;
	void receiveCarCollision(Player *car) override;

	ofPlanePrimitive plano;

	ofxAssimpModelLoader model;

	ofTexture texture;

};

#endif /* Oil_h */
