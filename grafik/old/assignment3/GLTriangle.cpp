/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/
#include "GLTriangle.h"

void GLTriangle::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    glBindBuffer(GL_ARRAY_BUFFER, m_VBO);
    glDrawArrays(GL_TRIANGLES, 0, m_numberOfVertices);

    glDisableVertexAttribArray(m_vertexPositionAttribute);
}

void GLTriangle::initializeBuffers(ShaderProgram & shaderProgram) {

	m_numberOfVertices = 3;

	float vertices[9] = { 
		-33.978017f, -34.985076f,  50.214926f,
		 84.192943f, -13.784394f, -50.214926f,
		-16.236910f,  83.754546f, -50.214926f
	};

	//Generate a vertex buffer object (m_VBO), and copy the array data (vertices) to it:
	glGenBuffers(1, &m_VBO);
	glBindBuffer(GL_ARRAY_BUFFER, m_VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

	//Get a handle to the shader attribute aPosition and set it up:
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}


