/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 07-Mar-2014
*/

#include <stdio.h>
#include <string>
#include <iostream>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include "glm/glm.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtx/transform2.hpp"

#include "ShaderProgram.h"
#include "GLCurve.h"

GLCurve curve;
bool hardwareShaderSupport;
ShaderProgram triangleShaderProgram;


static const std::string sVertexShader = "															  \n\
					#version 110                                                                      \n\
																									  \n\
					attribute vec3 aPosition;														  \n\
																									  \n\
					uniform mat4 uModelMatrix;														  \n\
					uniform mat4 uPerspectiveMatrix;												  \n\
																									  \n\
					void main() {                                                                     \n\
						gl_Position = uPerspectiveMatrix * uModelMatrix * vec4(aPosition.xyz, 1.0);	  \n\
					}																				  \n\
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



static glm::mat4 getPerspectiveProjectionMatrix(const glm::vec3& VRP,
									const glm::vec3& VPN,
									const glm::vec3& VUP,
									const glm::vec3& PRP,
									float cwUmax, float cwVmax,
									float cwUmin, float cwVmin,
									float frontClipping, float backClipping)
{	
	glm::vec3 CW((cwUmax + cwUmin) / 2.0f, (cwVmax + cwVmin) / 2.0f, 0.0f);
	
	GLfloat zMax = -((frontClipping - PRP.z)/(backClipping - PRP.z));

	glm::mat4 parper;
	parper[2][2] = 1.0f / (1.0f + zMax);
	parper[2][3] = -1.0f;
	parper[3][2] = -zMax / (1.0f + zMax);
	parper[3][3] = 0.0f;
	
	glm::mat4 scale = glm::scale(glm::mat4(), glm::vec3((-2.0f * PRP.z) / ((cwUmax - cwUmin) * (backClipping - PRP.z)),
				(-2.0f * PRP.z) / ((cwVmax - cwVmin) * (backClipping - PRP.z)),
				(-1.0f) / (backClipping - PRP.z)));
	
	glm::vec3 DOP = PRP - CW;
	glm::mat4 shear = glm::shearZ3D(glm::mat4(), -(DOP.x / DOP.z), -(DOP.y / DOP.z));

	glm::mat4 translationPRP = glm::translate(glm::mat4(), -PRP);

	glm::vec3 rz = glm::normalize(VPN);
	glm::vec3 rx = glm::normalize(glm::cross(VUP, rz));
	glm::vec3 ry = glm::normalize(glm::cross(rz, rx));
	glm::mat4 rotation = glm::mat4(glm::mat3(rx, ry, rz));

	glm::mat4 translationVRP = glm::translate(glm::mat4(), -VRP);

	return parper * scale * shear * translationPRP * rotation * translationVRP;
}

static void renderSceneCB() {
    glClear(GL_COLOR_BUFFER_BIT);

	triangleShaderProgram.useProgram();

	glm::mat4 modelMatrix;
	
	glm::vec3 vrp(0.0f, 0.0f, 125.0f);
	glm::vec3 vpn(0.0f, 0.0f, 1.0f);
	glm::vec3 vup(0.0f, 1.0f, 0.0f);
	glm::vec3 prp(0.0f,0.0f, 50.0f);
	float uMin = -25.0f, vMin = -25.0f;
	float uMax = 25.0f, vMax = 25.0f;
	float front = 10.0f, back = -800.0f;
	glm::mat4 perspectiveMatrix = getPerspectiveProjectionMatrix(vrp, vpn, vup, prp, uMax, vMax, uMin, vMin, front, back);
	
	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uModelMatrix"), 1, GL_FALSE, glm::value_ptr(modelMatrix));
	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uPerspectiveMatrix"), 1, GL_FALSE, glm::value_ptr(perspectiveMatrix));

	glUniform3f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uColour"), 1.0f, 1.0f, 0.0f);
	curve.draw();

    glutSwapBuffers();
}

static void initializeGlutCallbacks() {
    glutDisplayFunc(renderSceneCB);
}

static void intializeShadersAndGLObjects() {
	triangleShaderProgram.init(sVertexShader, sFragmentShader);
    curve.initializeBuffers(triangleShaderProgram);
}

int main(int argc, char** argv)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
    glutInitWindowSize(600, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Assignment 4");

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


    // glm::vec3 Ia(0.5f, 0.5f, 0.5f);
    // glm::vec3 Ip(1.0f, 1.0f, 1.0f);
    // glm::vec3 L (266.395325f, 274.291267f, -43.696048);
    // float Ka = 0.5f;
    // glm::vec3 Oa(0.f, 1.f, 0.f);
    // float Kd = 0.75f;
    // glm::vec3 Od(0.f, 1.f, 0.f);
    // float Ks = 0.9f;
    // glm::vec3 Os(1.f, 1.f, 1.f);
    // float Fatt = 1.0f;
    // int n = 20;
    // Phong(Ia, Ip, L, Ka, Oa, Kd, Od, Ks, Os, Fatt, n);

	intializeShadersAndGLObjects();

    glutMainLoop();

    return 0;
}
