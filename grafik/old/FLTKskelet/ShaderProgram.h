/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#pragma once

#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cstring>
#include <cctype>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <stdio.h>


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
