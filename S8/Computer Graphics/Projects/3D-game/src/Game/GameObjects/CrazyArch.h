#pragma once
#ifndef CrazyArch_h
#define CrazyArch_h
#include "GameObject.h"
#include "ofxAssimpModelLoader.h"

class CrazyArch : public GameObject {

public:
	CrazyArch(Game *game, glm::vec3 pos, glm::vec3 dim);
	~CrazyArch();

	void update() override;
	void draw() override;
	void receiveCarCollision(Player *car) override;

	ofPlanePrimitive plano;

	ofxAssimpModelLoader model;

	ofTexture texture;

	int archValue;
	int updateValue;
	int cooldown;
	int prize;

	ofImage image;
	ofImage image1;
	ofImage image2;
	ofImage image3;
	ofImage death;

	ofFbo fbo;

};

#endif /* Dirt_h */
