/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/
#include "GLTriangle.h"

void GLTriangle::draw() {
    std::cout << " draw  " << std::endl;
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    // glBindBuffer(GL_ARRAY_BUFFER, m_BackVBO);
	// glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
    // glDrawArrays(GL_LINE_LOOP, 0, m_numberOfVerticesBack);

    glBindBuffer(GL_ARRAY_BUFFER, m_FrontVBO);
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glDrawArrays(GL_LINE_LOOP, 0, m_numberOfVerticesFront);
	
	glDisableVertexAttribArray(m_vertexPositionAttribute);
}

void GLTriangle::initializeBuffers(ShaderProgram & shaderProgram) {

	m_numberOfVerticesFront = 5;
	m_numberOfVerticesBack = 5;

	float front[] = { 
		0.0f, 0.0f, 0.0f,
		16.0f, 0.0f, 0.0f,
		16.0f, 10.0f, 0.0f,
		8.0f, 16.0f, 0.0f,
		0.0f, 10.0f, 0.0f
	};

	// float back[] = {
		// 0.0f, 0.0f, 30.0f,
		// 16.0f, 0.0f, 30.0f,
		// 16.0f, 10.0f, 30.0f,
		// 8.0f, 16.0f, 30.0f,
		// 0.0f, 10.0f, 30.0f
	// };

	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");

	//Generate a vertex buffer object (m_VBO), and copy the array data (vertices) to it:
	// glGenBuffers(1, &m_BackVBO);
	// glBindBuffer(GL_ARRAY_BUFFER, m_BackVBO);
	// glBufferData(GL_ARRAY_BUFFER, sizeof(back), back, GL_STATIC_DRAW);
	// glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);

	glGenBuffers(1, &m_FrontVBO);
	glBindBuffer(GL_ARRAY_BUFFER, m_FrontVBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(front), front, GL_STATIC_DRAW);
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}


