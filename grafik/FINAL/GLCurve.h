/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#pragma once

#include <GL/glew.h>
#include <GL/freeglut.h>
#include "ShaderProgram.h"

class GLCurve
{
public:
	GLCurve() {}

	void draw();
	void initializeBuffers(ShaderProgram & shaderProgram);

	~GLCurve() {}

private: 
	GLuint m_Curve;
	int m_numberOfCurves;
    glm::vec3 flantnessTest(glm::vec4, float);
    void drawBezierSubdivision(glm::vec3[]); 

	GLuint m_vertexPositionAttribute;
};

