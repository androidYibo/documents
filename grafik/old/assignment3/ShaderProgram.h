/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#pragma once

#include <string>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <stdio.h>
#include <cstring>

class ShaderProgram
{
public:
	ShaderProgram() {}

	void init(std::string vs, std::string fs);
	void useProgram();

	inline GLuint getProgram() { return m_program; }

	~ShaderProgram() {};

private:
	GLuint m_program;

	void addShader (std::string &shaderString, GLenum shaderType);
	void compileShader(std::string &vs, std::string &fs);
};