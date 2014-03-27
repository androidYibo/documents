/*
	Author: Kasper Passov 
	Date: 17-Feb-2014
*/
#include "GLCurve.h"

void GLCurve::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glDrawArrays(GL_LINE_LOOP, 0, m_numberOfCurves);

	glDisableVertexAttribArray(m_vertexPositionAttribute);
}

void GLCurve::initializeBuffers(ShaderProgram & shaderProgram) {

	m_numberOfCurves = 4;

	float curve[] = {
		0.0f, 0.0f, 0.0f,
		4.0f, 5.0f, -5.0f,
		8.0f, -5.0f, 5.0f,
		12.0f, 0.0f, 0.0f
	};
    
    
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");

	glGenBuffers(1, &m_Curve);
	glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
	glBufferData(GL_ARRAY_BUFFER, m_numberOfCurves, curve, GL_STATIC_DRAW);
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}


