#ifndef Road_h
#define Road_h

#include "GameObject.h"
#include "Barrera.h"

class Road : public GameObject{
    
    ofLight light;
	ofLight light2;
	ofLight light3;
	ofLight light4;
	ofLight light5;

    ofPlanePrimitive plane;
public:
    Road(Game *game, glm::vec3 pos, glm::vec3 dim);
    ~Road();
    
    void draw() override;
    void drawDebug() override;
    void receiveBarreraCollision(Barrera* barrera) override;
    void setColor(ofColor color);
	void lightEnable1();
	void lightDisable1();
	void lightEnable2();
	void lightDisable2();
	void lightEnable3();
	void lightDisable3();
	void lightEnable4();
	void lightDisable4();
	void lightEnable5();
	void lightDisable5();

    
};

#endif /* Road_h */
