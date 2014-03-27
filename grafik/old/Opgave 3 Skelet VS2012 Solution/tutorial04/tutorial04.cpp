/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#include <stdio.h>
#include <string>
#include <iostream>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include "glm/glm.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/type_ptr.hpp"

#include "GLTriangle.h"
#include "ShaderProgram.h"

GLTriangle triangle;
bool hardwareShaderSupport;
ShaderProgram triangleShaderProgram;

static const std::string sVertexShader = "															 \n\
					#version 110                                                                     \n\
																									 \n\
					attribute vec3 aPosition;														 \n\
																									 \n\
					uniform mat4 uModelMatrix;														 \n\
					uniform mat4 uProjectionMatrix;													 \n\
																									 \n\
					void main() {                                                                    \n\
						gl_Position = uProjectionMatrix * uModelMatrix * vec4(aPosition.xyz, 1.0);	 \n\
					}																				 \n\
";

static const std::string sFragmentShader = "														  \n\
					#version 110																	  \n\
																									  \n\
					uniform vec3 uColour;															  \n\
																									  \n\
					void main()	{																	  \n\
						gl_FragColor = vec4(uColour, 1.0);											  \n\
					}																				  \n\
";

static void renderSceneCB() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	triangleShaderProgram.useProgram();	

	glm::mat4 modelMatrix;
	modelMatrix = glm::translate(modelMatrix, glm::vec3(0.f, 0.0f, -105.0f));
	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uModelMatrix"), 1, GL_FALSE, glm::value_ptr(modelMatrix));
	
	glUniform3f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uColour"), 0.0f, 1.0f, 0.0f);
	triangle.draw();

    glutSwapBuffers();
}


static void initializeGlutCallbacks() {
    glutDisplayFunc(renderSceneCB);
}

static void intializeShadersAndGLObjects() {
	triangleShaderProgram.init(sVertexShader, sFragmentShader);
	triangle.initializeBuffers(triangleShaderProgram);

	triangleShaderProgram.useProgram();
	glm::mat4 projectionMatrix = glm::perspective(45.0f, 4.0f / 3.0f, 0.1f, 100.f); 
	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uProjectionMatrix"), 1, GL_FALSE, glm::value_ptr(projectionMatrix));
}

int main(int argc, char** argv)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
    glutInitWindowSize(600, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Assignment 2");

    initializeGlutCallbacks();

    // Must be done after glut is initialized!
    GLenum res = glewInit();
    if (res != GLEW_OK) {
      fprintf(stderr, "Error: '%s'\n", glewGetErrorString(res));
      return 1;
    }

	hardwareShaderSupport = GLEW_VERSION_2_0;
	if (!hardwareShaderSupport) {
		std::cout << "Your graphics hardware/driver setup does not support OpenGL 2.0+." << std::endl;
		std::cout << "Your setup supports OpenGL: " << glGetString(GL_VERSION) << std::endl;
		std::cout << "Press enter to exit" << std::endl;
		std::cin.ignore();
		exit(EXIT_FAILURE);
	}

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

	glEnable(GL_DEPTH_TEST);

	intializeShadersAndGLObjects();

    glutMainLoop();

    return 0;
}