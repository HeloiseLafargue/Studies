#include "Rail.hpp"



void Rail::setId(int newId) {
	id = newId;
}

int Rail::getId() {
	return id;
}

void Rail::setWidth(int newWidth) {
	width = newWidth;
}

int Rail::getWidth() {
	return width;
}

void Rail::setPos(int newPos) {
	pos = newPos;
}

int Rail::getPos() {
	return pos;
}

void Rail::setY_coord(int newY_coord) {
	y_coord = newY_coord;
}

int Rail::getY_coord() {
	return y_coord;
}

void Rail::setOcuppied(bool newOccupied) {
	occupied = newOccupied;
}

bool Rail::getOccupied() {
	return occupied;
}

void Rail::setLine(Line* newLine) {
	line = newLine;
}

Line* Rail::getLine() {
	return line;
}