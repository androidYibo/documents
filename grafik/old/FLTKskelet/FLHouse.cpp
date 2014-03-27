/*
	Author: Kasper Passov
	Date: Penis 
*/
#include "GLHouse.h"

void GLHouse::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    glBindBuffer(GL_ARRAY_BUFFER, m_VBO);
    glDrawArrays(GL_TRIANGLES, 0, m_numberOfVertices);

    glDisableVertexAttribArray(m_vertexPositionAttribute);
}

void GLHouse::initializeBuffers(ShaderProgram & shaderProgram) {

    float vertices[] = { 
        0.0f, 0.0f,  0.54f,
        0.16f, 0.0f,  0.54f,
        0.16f, 0.10f, 0.54f,
        0.08f, 0.16f, 0.54f,
        0.0f, 0.1f,  0.54f
    };

    int const numberOfVertices  = sizeof(vertices) / sizeof(float);


	//Generate a vertex buffer object (m_VBO), and copy the array data (vertices) to it:
	glGenBuffers(1, &m_VBO);
	glBindBuffer(GL_ARRAY_BUFFER, m_VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

	//Get a handle to the shader attribute aPosition and set it up:
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}


