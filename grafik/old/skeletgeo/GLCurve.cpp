/*
	Author: Kasper Passov 
	Date: 17-Feb-2014
*/
#include "GLCurve.h"

void GLCurve::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
    glDrawArrays(GL_TRIANGLES, 0, m_numberOfCurves);

    glDisableVertexAttribArray(m_vertexPositionAttribute);
}

// void drawBezierSubdivision(Point G1, Point G2, Point G3, Point G4, double n) {
    // if (n > 0){
        // Point Left[4];
        
        // Left[0] = G1;
        // Left[1] = (G1 * 4 + G2 * 4)/8;
        // Left[2] = (G1 * 2 + G2 * 4 + G3 * 2)/8;
        // Left[3] = (G1     + G2 * 3 + G3 * 3 + G4)/8;

        // drawBezierSubdivision(Left[0], Left[1], Left[2], Left[3], floor(n/2)); // Calculates and draws the left side by creating a subcurve


        // P = Left[3];
        // drawLine(POld, P); // Draws the middle line of the current curve
        // POld = P;


        // Point Right[4];

        // Right[0] = G4;
        // Right[1] = (G4 * 4 + G3 * 4)/8;
        // Right[2] = (G4 * 2 + G3 * 4 + G2 * 2)/8;
        // Right[3] = (G4 + G3 * 3 + G2 * 3 + G1)/8;

        // drawBezierSubdivision(Right[3], Right[2], Right[1], Right[0], ceil(n/2)-1); // Calculates and draws the right side
    // }
// }


void GLCurve::initializeBuffers(ShaderProgram & shaderProgram) {

	m_numberOfCurves = 3;

	float curve[9] = { 
		-33.978017f, -34.985076f,  50.214926f,
		 84.192943f, -13.784394f, -50.214926f,
		-16.236910f,  83.754546f, -50.214926f
	};
    
    
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");

	//Generate a vertex buffer object (m_VBO), and copy the array data (vertices) to it:
	glGenBuffers(1, &m_Curve);
	glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
	glBufferData(GL_ARRAY_BUFFER, sizeof(curve), curve, GL_STATIC_DRAW);

	//Get a handle to the shader attribute aPosition and set it up:
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}


