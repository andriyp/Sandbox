#include <GL/freeglut.h>
#include <GL/gl.h>
#include "Triangle2d.h"

void Triangle2d::render () {    
    glBegin(GL_LINE_LOOP);
        glVertex2f(this->ptA.x, this->ptA.y);
        glVertex2f(this->ptB.x, this->ptB.y);
        glVertex2f(this->ptC.x, this->ptC.y);
    glEnd();
}