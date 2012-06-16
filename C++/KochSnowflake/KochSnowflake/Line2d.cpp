#include "Point2d.h"
#include "Line2d.h"
#include <math.h>

#define sqr(x) ((x)*(x))

Point2d line2dMiddle(Point2d ptA, Point2d ptB) {
	return Point2d(
		(ptA.x + ptB.x) / 2,
		(ptA.y + ptB.y) / 2
	);
}

int line2dLength (Point2d ptA, Point2d ptB) {
	return (int)sqrt((float)sqr(ptA.x - ptB.x) + (float)sqr(ptA.y - ptB.y));
}
