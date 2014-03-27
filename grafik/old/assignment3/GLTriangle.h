/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#pragma once

#include <GL/glew.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#include "ShaderProgram.h"

class GLTriangle
{
public:
	GLTriangle() {}

	void draw();
	void initializeBuffers(ShaderProgram & shaderProgram);

	~GLTriangle() {}

private: 
	GLuint m_VBO;
	int m_numberOfVertices;

	GLuint m_vertexPositionAttribute;
};

