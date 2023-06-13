#include "Line.hpp"

void Line::draw() {

	poli.lineTo(x, y);
	poli.lineTo(x+lon, y);

	poli.draw();
	
}