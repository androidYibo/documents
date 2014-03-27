#pragma comment(lib,"glew32.lib")

// Code based on StackOverflow 7884433
// http://stackoverflow.com/questions/7884433/drawing-bezier-curves-using-de-casteljau-algorithm-in-c-opengl

#include <stdlib.h>
#include <GL/freeglut.h>
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>


int SCREEN_HEIGHT = 480;
// Keep track of times clicked, on 3 clicks draw.
int NUMPOINTS = 0;

// Point class to keep it a little cleaner.
class Point {
public:
    float x, y, z;
    void setxy(float x2, float y2) { setxyz(x2,y2,0); }
    void setxyz(float x2, float y2, float z2) { x = x2; y = y2; z = z2;}
    const Point & operator=(const Point &rPoint) {
         x = rPoint.x;
         y = rPoint.y;
         z = rPoint.z;

         return *this;
      }

};

Point abc[4];

void myInit() {
    glClearColor(0.0,0.0,0.0,0.0);
    glColor3f(1.0,0.0,0.0);
    glPointSize(4.0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0.0,640.0,0.0,480.0);

}

void drawDot(int x, int y) {
    glBegin(GL_POINTS);
     glVertex2i(x,y);
    glEnd();
    glFlush();
}

void drawLine(Point p1, Point p2) {
    glBegin(GL_LINES);
      glVertex3f(p1.x, p1.y, p1.z);
      glVertex3f(p2.x, p2.y, p2.z);

    glEnd();
    glFlush();
}

// Calculate the next bezier point via sampling.
Point drawBezierSample(Point A, Point B, Point C, Point D, double t) {
    Point P;
    P.x = pow((1 - t), 3) * A.x + 3 * t * pow((1 -t), 2) * B.x + 3 * (1-t) * pow(t, 2)* C.x + pow (t, 3)* D.x;
    P.y = pow((1 - t), 3) * A.y + 3 * t * pow((1 -t), 2) * B.y + 3 * (1-t) * pow(t, 2)* C.y + pow (t, 3)* D.y;
	P.z = 0;
    return P;
}

// Calculate the next bezier point via forward differences.
Point drawBezierForwardDiff(Point deltas[]) {
    Point P = deltas[0];
	return P;
}

//Use this to initialize the matrix {f0, delf0, del^2f0, del^3f0} in array deltas[]
void forwardDiffInit(Point A0, Point B0, Point C0, Point D0, double del, Point deltas[]){
}

// Calculate the next bezier point via subdivision.
Point drawBezierSubdivision(Point Left[], Point Right[]) {
    Point P = Left[0];
	return P;
}

void drawBezierLine(int style, Point A, Point B, Point C, Point D, double n){
	
        Point POld = abc[0];

		//Use this for BezierSampling!
		if(style == 1){
        for(double t = 0.0;t <= 1.0; t += 1.0/n) {
            Point P = drawBezierSample(abc[0], abc[1], abc[2], abc[3],  t);
            drawLine(POld, P);
            POld = P;
		}
		}
		
		//Use this for BezierForwardDiff!
		else if(style == 2){
			Point dels[4];
			forwardDiffInit(abc[0],abc[1],abc[2],abc[3],1.0/n,dels);
			POld = dels[0];
        for(double t = 0.0;t <= 1.0; t += 1.0/n) {
            Point P = drawBezierForwardDiff(dels);
            drawLine(POld, P);
            POld = P;
		}
		}

		
		//Use this for BezierSubdivision!
		else{
			Point Left[4];
			Point Right[4];
        for(double t = 0.0;t <= 1.0; t += 1.0/n) {
            Point P = drawBezierSubdivision(Left, Right);
            drawLine(POld, P);
            POld = P;
		}
		}
        glColor3f(1.0,0.0,0.0);
}

void myMouse(int button, int state, int x, int y) {
  // If left button was clicked
  if(button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
      // Store where the user clicked, note Y is backwards.
    abc[NUMPOINTS].setxy((float)x,(float)(SCREEN_HEIGHT - y));
    NUMPOINTS++;

    // Draw the red  dot.
    drawDot(x, SCREEN_HEIGHT - y);

    // If 3 points are drawn do the curve.
    if(NUMPOINTS == 4) {
        glColor3f(0,1.0,0);
        drawLine(abc[0], abc[1]);
        drawLine(abc[1], abc[2]);
        drawLine(abc[2], abc[3]);
        glColor3f(1.0,1.0,1.0);

		//This line is where we specify everything
		//the first parameter is which style:
		// 1 = Sampling
		// 2 = Forward Differentiation
		// 3 = Subdivision
		// and the last parameter is how many subdivisions
		drawBezierLine(1,abc[0], abc[1], abc[2], abc[3],  6);
        
		
		NUMPOINTS = 0;
    }
  }
}

void myDisplay() {
//    glClear(GL_COLOR_BUFFER_BIT);
//    glFlush();
}

int main(int argc, char *argv[]) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB);
    glutInitWindowSize(640,480);
    glutInitWindowPosition(100,150);
    glutCreateWindow("Bezier Curve");

    glutMouseFunc(myMouse);
    glutDisplayFunc(myDisplay);

    myInit();
    glutMainLoop();

    return 0;
}
