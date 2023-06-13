//
//  Obstacle.hpp
//  flipsideFury
//
//  Created by Hanjie Zhu on 17/3/23.
//

#ifndef Obstacle_hpp
#define Obstacle_hpp

#include <stdio.h>
#include "Rail.hpp"

class Obstacle{

    public:
        int type;
        int x_coord;
        int speed;

        Rail* rail;
        Obstacle(){};
        Obstacle(int t, int x, int s, Rail* r) { // Constructor with parameters
            type = t;
            x_coord = x;
            speed = s;
            rail = r;
        }
    
    void draw();
    void setRail(Rail* newRail);
    Rail* getRail();
    
    void setType(int newtype);
    int getType();
    
    void setXcoord(int newXcoord);
    int getXcoord();
    
    void setSpeed(int newSpeed);
    int getSpeed();
    
};

#endif /* Obstacle_hpp */
