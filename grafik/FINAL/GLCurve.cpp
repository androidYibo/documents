/*
	Author: Kasper Passov 
	Date: 17-Feb-2014
*/  
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/matrix_access.hpp"

#include "GLCurve.h"


void GLCurve::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
   glDrawArrays(GL_TRIANGLES, 0, m_numberOfCurves);

    glDisableVertexAttribArray(m_vertexPositionAttribute);
}

glm::vec3 flatnessTest(glm::vec3 G1, glm::vec3 G4, glm::vec3 x)
    {
        glm::vec3 xG1 = x - G1;
        glm::vec3 G4G1 = G4 - G1;
        glm::vec3 norm = (G4G1/glm::normalize(G4G1));
        return glm::normalize(xG1 - glm::cross(xG1, norm) * norm);
    }


void drawBezierSubdivision(glm::vec3 G[]) {
    // if (max(flatnessTest(G[2]), flatnessTest(G[3]))<1){
    if (true){
        glm::vec3 v2 (2.f,2.f,2.f);
        glm::vec3 v3 (3.f,3.f,3.f);
        glm::vec3 v4 (4.f,4.f,4.f);
        glm::vec3 v8 (8.f,8.f,8.f);
        glm::vec3 Left[4] (G[1], 
                          (G[1] * v4 + G[2] * v4)/v8,
                          (G[1] * v2 + G[2] * v4 + G[3] * v2)/v8,
                          (G[1]      + G[2] * v3 + G[3] * v3 + G[4])/v8);

        drawBezierSubdivision(Left); // Calculates and draws the left side by creating a subcurve

        // drawLine(POld, Left[4]); // Draws the middle line of the current curve
        // POld = Left[4];

        glm::vec3 Right[4] (                               G[4], 
                                              (G[3] * v4 + G[4] * v4)/v8,
                                  (G[2] * v2 + G[3] * v4 + G[4] * v2)/v8,
                           (G[1] + G[2] * v3 + G[3] * v3 + G[4]     )/v8);

        drawBezierSubdivision(Right); // Calculates and draws the right side
    }
}


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


