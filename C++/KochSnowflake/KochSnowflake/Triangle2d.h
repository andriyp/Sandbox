#ifndef TRIANGLE2D_H
#define TRIANGLE2D_H

#include "Point2d.h"

class Triangle2d {
public:
	Point2d ptA, ptB, ptC;

	Triangle2d (Point2d ptA, Point2d ptB, Point2d ptC)
		: ptA(ptA)
		, ptB(ptB)
		, ptC(ptC)
	{}

	void render ();
};

#endif