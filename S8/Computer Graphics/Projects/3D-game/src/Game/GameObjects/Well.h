#pragma once
#pragma once
#ifndef Well_h
#define Well_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class Well : public GameObject {

public:
	Well(Game *game, glm::vec3 pos, glm::vec3 dim);
	~Well();

	void update() override;
	void draw() override;
	void receiveCarCollision(Player *car) override;

	ofPlanePrimitive plano;

	ofxAssimpModelLoader model;

	ofTexture texture;

};

#endif /* Well_h */
