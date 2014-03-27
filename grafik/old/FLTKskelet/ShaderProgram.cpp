/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#include "ShaderProgram.h"

void ShaderProgram::init(std::string vs, std::string fs) {
	compileShader(vs, fs);
}

void ShaderProgram::compileShader (std::string &vs, std::string &fs) {
	m_program = glCreateProgram();

	if (m_program == 0) {
		fprintf(stderr, "Error creating shader program\n");
		exit(-1);
	}
	
	addShader(vs, GL_VERTEX_SHADER);
	addShader(fs, GL_FRAGMENT_SHADER);
	
	GLint success = 0;
	GLchar ErrorLog[1024] = { 0 };

	glLinkProgram(m_program);
	glGetProgramiv(m_program, GL_LINK_STATUS, &success);
	if (success == 0) {
		glGetProgramInfoLog(m_program, sizeof(ErrorLog), NULL, ErrorLog);
		fprintf(stderr, "Error linking shader program: '%s'\n", ErrorLog);
		exit(-1);
	}
	
	glValidateProgram(m_program);
	glGetProgramiv(m_program, GL_VALIDATE_STATUS, &success);
	if (!success) {
		glGetProgramInfoLog(m_program, sizeof(ErrorLog), NULL, ErrorLog);
		fprintf(stderr, "Invalid shader program: '%s'\n", ErrorLog);
		exit(-1);
	}
}

void ShaderProgram::addShader (std::string &shaderString, GLenum shaderType) {
	  GLuint shaderObj = glCreateShader(shaderType);

		if (shaderObj == 0) {
			fprintf(stderr, "Error creating shader type %d\n", shaderType);
			exit(-1);
		}

		const GLchar* p[1];
		p[0] = shaderString.c_str();
		GLint Lengths[1];
		Lengths[0]= strlen(p[0]);
		glShaderSource(shaderObj, 1, p, Lengths);
		glCompileShader(shaderObj);
		GLint success;
		glGetShaderiv(shaderObj, GL_COMPILE_STATUS, &success);
		if (!success) {
			GLchar InfoLog[1024];
			glGetShaderInfoLog(shaderObj, 1024, NULL, InfoLog);
			fprintf(stderr, "Error compiling shader type %d: '%s'\n", shaderType, InfoLog);
			exit(-1);
		}
		glAttachShader(m_program, shaderObj);
  }

void ShaderProgram::useProgram() {
	glUseProgram(m_program);
}


