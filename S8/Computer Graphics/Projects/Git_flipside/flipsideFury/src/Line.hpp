#pragma once

#include "ofMain.h"

class Line {

	public:
		int id;
		int x; // 0 para superior, 1 para inferior
		int y;
		int lon;
		
		Line(int i, int x_val, int y_val, int lon_val) { // Constructor with parameters
			id = i;
			x = x_val;
			y = y_val;
			lon = lon_val;
		}
		void draw();


		ofPolyline poli;
};
