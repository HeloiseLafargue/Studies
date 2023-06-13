#pragma once
#ifndef ColliderLaps_h
#define ColliderLaps_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class ColliderLaps : public GameObject {

public:
	ColliderLaps(Game *game, glm::vec3 pos, glm::vec3 dim);
	~ColliderLaps();

	void update() override;
	void draw() override;
	void receiveCarCollision(Player *car) override;

	ofPlanePrimitive plano;

	ofxAssimpModelLoader model;

	ofTexture texture;

};

#endif /* ColliderLaps_h */
