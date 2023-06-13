#pragma once

#include "ofMain.h"
#include "Line.hpp"

class Rail{

	public:
		int id;
		int width;
		int pos; // 0 para superior, 1 para inferior
		int y_coord;
		bool occupied;

		Line* line;
		
		Rail(int i, int p, int w, int y, bool o, Line* l) { // Constructor with parameters
			id = i;
			width = w;
			pos = p;
			y_coord = y;
			occupied = o;
			line = l;
		}


		void setId(int newId);
		int getId();

		void setWidth(int newWidth);
		int getWidth();

		void setPos(int newPos);
		int getPos();

		void setY_coord(int newY_coord);
		int getY_coord();

		void setOcuppied(bool newOccupied);
		bool getOccupied();

		void setLine(Line* newLine);
		Line* getLine();
};
