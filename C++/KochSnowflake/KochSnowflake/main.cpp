#define _USE_MATH_DEFINES

#include <GL/freeglut.h>
#include <math.h>
#include "Point2d.h"
#include "Triangle2d.h"

#define _WIN32_WINNT 0x0500

double rad (double angle) {
	return angle * M_PI / 180;
}

const float
	cos60 = cosf(rad(60)),
	sin60 = sinf(rad(60)),
	tan30 = tanf(rad(30)),
	tan60 = tanf(rad(60));

Triangle2d kochStep (Point2d ptA, Point2d ptB) {
	float dABx = ptB.x - ptA.x,
		  dABy = ptB.y - ptA.y;

	Point2d
		newPtA(ptA.x + dABx / 3, ptA.y + dABy / 3),
		newPtB(ptB.x - dABx / 3, ptB.y - dABy / 3);

	float dx = newPtB.x - newPtA.x,
		  dy = newPtB.y - newPtA.y;

	Point2d newPtC(
		newPtA.x + dx * cos60 - dy * sin60,
		newPtA.y + dy * cos60 + dx * sin60
	);

	return Triangle2d(newPtA, newPtB, newPtC);
}

void renderKoch (int iterations, Triangle2d tri) {
	if (!iterations) return;

	tri.render();
	
	renderKoch(iterations - 1, kochStep(tri.ptB, tri.ptA));
	renderKoch(iterations - 1, kochStep(tri.ptC, tri.ptB));
	renderKoch(iterations - 1, kochStep(tri.ptA, tri.ptC));
}

void initScene (int width, int height) {
	glClearColor(1, 1, 1, 1);
	glMatrixMode(GL_MODELVIEW);
	glOrtho(0, 1024, 0, 768, -1024, 1024);

	glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	glShadeModel(GL_SMOOTH);
	glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
	glEnable(GL_LINE_SMOOTH);
}

void display () {
	static float rot = 0.;
	static int iter = 3;

	glClear(GL_COLOR_BUFFER_BIT);

	glPushMatrix();

	glTranslatef(500, 200 + 300 * tan30, 0);	
	glRotatef(rot += 0.05, 0, 0, 1);	
	glTranslatef(-500, -(200 + 300 * tan30), 0);

	glColor3b(200, 100, 50);

	renderKoch(2 + ((iter += 5) %= 50000) / 10000,
		Triangle2d(
			Point2d(200, 200),
			Point2d(800, 200),
			Point2d(500, 200 + 300 * tan60)
		)
	);
	
	glPopMatrix();
	glFlush();

	Sleep(1);
}

int main (int argc, char **argv) {	
	ShowWindow(GetConsoleWindow(), SW_HIDE);

	int w = 1024, h = 768;

	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE);
	glutInitWindowPosition(400, 100);
	glutInitWindowSize(w, h);
	glutCreateWindow("Koch Snoflake 2d");
	initScene(w, h);

	glutIdleFunc(display);
	glutDisplayFunc(display);

	glutMainLoop();
}