/*
	Author: Kasper Passov 
*/

#include <stdio.h>
#include <string>
#include <iostream>
#include <iomanip>
#include <vector>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include "glm/glm.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtx/transform2.hpp"

#include "ShaderProgram.h"
#include "GLPatch.h"

#define _USE_MATH_DEFINES                                            

GLPatch patch;
// std::vector<GLPatch> buffers;
// std::vector<glm::vec3> normals;
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
				    varying vec4 vPos;              												  \n\
				    varying mat4 vMat;              												  \n\
																									  \n\
					void main() {                                                                     \n\
                        vPos = vec4(aPosition.xyz, 1.0);                                              \n\
                        vMat = uModelMatrix;                                                          \n\
						gl_Position = uPerspectiveMatrix * uModelMatrix * vec4(aPosition.xyz, 1.0);	  \n\
					}																				  \n\
";

static const std::string sFragmentShader = "														  \n\
					#version 110																	  \n\
																									  \n\
				    // from code																	  \n\
					uniform vec4 uAmb, uDif, uSpe, uNormal, uLightPos, uViewPos;	         		  \n\
				    uniform int un;																	  \n\
																									  \n\
				    // from vertexShader															  \n\
                    varying vec4 vPos;                                                                \n\
                    varying mat4 vMat;                                                                \n\
																									  \n\
					void main()	{																	  \n\
                        vec4 V = normalize(uViewPos * vMat - vPos);                                   \n\
                        vec4 L = normalize(uLightPos * vMat - vPos);                                  \n\
                        vec4 R = reflect(-L, uNormal);                                                \n\
                        gl_FragColor = uAmb + uDif * dot(L, uNormal) + uSpe * pow(dot(R, V), un);     \n\
					}																				  \n\
";

static void Phong(const glm::vec3& Ia, // vec3 ambient light source(Ired, Igreen, Iblue) 
                  const glm::vec3& Ip, // vec3 point intensity (Ipred, Ipgreen, Ipblue)
                  const glm::vec3& Lp, // light source position
                  const glm::vec3& PRP, 
                  const glm::vec3& VRP,
                  ///////////////////  material properties
                  float Ka, const glm::vec3& Oa, 
                  float Kd, const glm::vec3& Od, 
                  float Ks, const glm::vec3& Os, 
                  float Fatt, float n) 
{
    glUniform4f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uLightPos"), Lp[0], Lp[1], Lp[2], 1.0);
    glUniform1i(glGetUniformLocation(triangleShaderProgram.getProgram(), "un"), n);
    

    glm::vec3 ep = PRP + VRP;                                      
    glUniform4f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uViewPos"), ep[0], ep[1], ep[2], 1.0);

    glm::vec3 amb (Ka * Od * Ia);
    glUniform4f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uAmb"), amb[0], amb[1], amb[2], 1.0);


    glm::vec3 fattv (Fatt, Fatt, Fatt);
    glm::vec3 dif (Kd * Od * fattv * Ip);
    glUniform4f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uDif"), dif[0], dif[1], dif[2], 1.0);


    glm::vec3 spe (Ks * Os * fattv * Ip);
    glUniform4f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uSpe"), spe[0], spe[1], spe[2], 1.0);
    
    // std::cout << "ambient = ("  << amb[0] << ", " << amb[1] << ", " << amb[2] << ")" << std::endl;   
    // std::cout << "diffuse = ("  << dif[0] << ", " << dif[1] << ", " << dif[2] << ")" << std::endl;   
    // std::cout << "specular = (" << spe[0] << ", " << spe[1] << ", " << spe[2] << ")" << std::endl;   
    // std::cout << "N = ("        << N[0]   << ","  << N[1]   << ", " << N[2]   << ")" << std::endl;
    // std::cout << "Eyepos = ("   << ep[0]  << ","  << ep[1]  << ", " << ep[2]  << ")" << std::endl;
    // std::cout << "n = "         << n      <<                                            std::endl;
}

std::ostream& operator << (std::ostream& s, glm::vec3 const& v) {
    s << " ";
    for(int i = 0; i < 3; ++i) {
        s << std::setw(6) << std::setprecision(4) << v[i] << " ";
    }
   s << std::endl;
   return s;
}


std::ostream& operator << (std::ostream& s, glm::mat4 const& v) { 
    for(int i = 0; i < 4; ++i) { 
        s << " "; 
        for(int t = 0; t < 4; ++t) { 
            s << std::setw(6) << std::setprecision(4) << v[t][i] << " "; 
        } 
        s << std::endl; 
    } return s; 
}

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
	
    glm::vec3 vrp(5.0f, 0.0f, 5.0f);
    glm::vec3 vpn(std::cos((30.f * M_PI)/180.f), 0.0f, std::sin((30.f * M_PI)/180.f));
    glm::vec3 vup(0.0f, 0.0f, 1.0f);
    glm::vec3 prp(0.0f,0.0f, 20.0f);
    float uMin = -4.0f, vMin = -4.0f;
    float uMax = 4.0f, vMax = 4.0f;
    float front = 40.0f, back = -10.0f;

    //triangle
    // glm::vec3 vrp(0.0f, 0.0f, 125.0f);
    // glm::vec3 vpn(0.0f ,0.0f, 1.0f); 
    // glm::vec3 vup(0.0f, 1.0f, 0.0f);
    // glm::vec3 prp(0.0f,0.0f, 50.0f);
    // float uMin = -25.0f, vMin = -25.0f;
    // float uMax = 25.0f, vMax = 25.0f;
    // float front = 10.0f, back = -800.0f;
    
    glm::mat4 perspectiveMatrix = getPerspectiveProjectionMatrix(vrp, vpn, vup, prp, uMax, vMax, uMin, vMin, front, back);

	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uModelMatrix"), 1, GL_FALSE, glm::value_ptr(modelMatrix));
	glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uPerspectiveMatrix"), 1, GL_FALSE, glm::value_ptr(perspectiveMatrix));

    //Light
    glm::vec3 Ia(0.5f, 0.5f, 0.5f);
    glm::vec3 Ip(1.0f, 1.0f, 1.0f);
    glm::vec3 Lpos (266.395325f, 274.291267f, -43.696048);

    float Ka = 0.5f;
    glm::vec3 Oa(0.f, 1.f, 0.f);
    float Kd = 0.75f;
    glm::vec3 Od(0.f, 1.f, 0.f);
    float Ks = 0.9f;
    glm::vec3 Os(1.f, 1.f, 1.f);

    float Fatt = 1.0f;
    int n = 20;

    Phong(Ia, Ip, Lpos, prp, vrp, Ka, Oa, Kd, Od, Ks, Os, Fatt, n);

    // for (int i = 0; i < buffers.size(); i++)
    // {
        // glm::vec3 Normal = glm::cross((patches.at(i)[0][0] - patches.at(i)[0][3]),(patches.at(i)[0][3] - patches.at(i)[3][3]));
        // glm::vec3 Nor = normals.at(i);
        // glm::vec3 N = glm::normalize(glm::vec3(Nor[0],Nor[1],Nor[2]+0.00001f));

        // glUniform4f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uNormal"), N[0], N[1], N[2], 1.0);
    // buffers.at(i).draw();
    // }
    patch.draw();
    glutSwapBuffers();
}

static void initializeGlutCallbacks() {
    glutDisplayFunc(renderSceneCB);
}

static void intializeShadersAndGLObjects() {
	triangleShaderProgram.init(sVertexShader, sFragmentShader);

    // std::vector<BezierPatch> patches;
    // ReadBezierPatches("data/teapot.data", patches);

    // BezierPatch G(glm::vec3(-1.0f,  1.0f, 0.0f), glm::vec3(-0.5f,  1.0f, 0.0f),
                  // glm::vec3( 0.5f,  1.0f, 0.0f), glm::vec3( 1.0f,  1.0f, 0.0f),
                  // glm::vec3(-1.0f,  0.5f, 0.0f), glm::vec3(-0.5f,  0.5f, 1.0f),
                  // glm::vec3( 0.5f,  0.5f, 1.0f), glm::vec3( 1.0f,  0.5f, 0.0f),
                  // glm::vec3(-1.0f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f),
                  // glm::vec3( 0.5f, -0.5f, 1.0f), glm::vec3( 1.0f, -0.5f, 0.0f),
                  // glm::vec3(-1.0f, -1.0f, 0.0f), glm::vec3(-0.5f, -1.0f, 0.0f),
                  // glm::vec3( 0.5f, -1.0f, 0.0f), glm::vec3( 1.0f, -1.0f, 0.0f));
    // patches.push_back(G);
    // for (int i = 0; i < patches.size(); i++)
    // {
        // patch.initializeBuffers(triangleShaderProgram, patches.at(i));
        // glm::vec3 Normal = glm::cross((patches.at(i)[0][0] - patches.at(i)[0][3]),(patches.at(i)[0][3] - patches.at(i)[3][3]));
        // normals.push_back(Normal);
        // buffers.push_back(patch);
    // }
    patch.initializeBuffers(triangleShaderProgram);

}

int main(int argc, char** argv)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
    glutInitWindowSize(600, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Final Assignment");

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
