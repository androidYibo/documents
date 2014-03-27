/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#pragma once

#include <GL/glew.h>
#include <GL/freeglut.h>
#include "ShaderProgram.h"

class GLTriangle
{
public:
	GLTriangle() {}

	void draw();
	void initializeBuffers(ShaderProgram & shaderProgram);

	~GLTriangle() {}

private: 
	GLuint m_BackVBO, m_FrontVBO;
	int m_numberOfVerticesBack, m_numberOfVerticesFront;

	GLuint m_vertexPositionAttribute;
};

