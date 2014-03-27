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
																			 \n\
					uniform mat4 uPerspectiveMatrix;														 \n\
					uniform mat4 uModelMatrix;														 \n\
																									 \n\
					void main() {                                                                    \n\
						gl_Position = uPerspectiveMatrix * uModelMatrix * vec4(aPosition.xyz, 1.0);	 \n\
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
    glClear(GL_COLOR_BUFFER_BIT);

	triangleShaderProgram.useProgram();

	glm::mat4 modelMatrix;
	modelMatrix = glm::translate(modelMatrix, glm::vec3(0.0f, 0.0f, -100.0f));	
	//modelMatrix = glm::scale(modelMatrix, glm::vec3(0.02));
	//modelMatrix = glm::translate(modelMatrix, glm::vec3(0.0f, 0.5f, 0.0f));	
	
	glm::mat4 perspectiveMatrix = glm::perspective(45.f, 1.f, 0.1f, 100.f);
	
	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uPerspectiveMatrix"), 1, GL_FALSE, glm::value_ptr(perspectiveMatrix));
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

	intializeShadersAndGLObjects();

    glutMainLoop();

    return 0;
}
