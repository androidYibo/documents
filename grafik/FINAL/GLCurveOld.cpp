/*
	Author: Kasper Passov 
	Date: 17-Feb-2014
*/  
#include <iostream>

#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/matrix_access.hpp"

#include "GLCurve.h"


void GLCurve::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);

    glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
   glDrawArrays(GL_TRIANGLES, 0, m_numberOfCurves);

    glDisableVertexAttribArray(m_vertexPositionAttribute);
}

double flatnessTest2(glm::vec3 G1, glm::vec3 G4, glm::vec3 x)
    {
        glm::vec3 xG1 = x - G1;
        glm::vec3 G4G1 = G4 - G1;
        glm::vec3 norm = glm::normalize(G4G1);
        return glm::length(xG1 - glm::dot(xG1, norm) * norm);
    }

// double flatnessTest(glm::vec3 G1, glm::vec3 G4, glm::vec3 x)
bool flatnessTest(glm::vec3 G[])
    {
        double test1 = flatnessTest2(G[0], G[3], G[1]); 
        double test2 = flatnessTest2(G[0], G[3], G[2]);
        // std::cout << "flatness tes" << std::endl;
        // std::cout << "test1 = " << test1 << std::endl;
        // std::cout << "test2 = " << test2 << std::endl;
        // std::cout << "" << std::endl;
        return (std::max(test1, test2)>0.001);
    }

void addtolist(BezierPatch G, glm::vec3 res[], int i)
{
    for(int j = 0; j < 4; j++){
        for(int k = 0; k < 4; k++){
            res[i] = G[j][k];
            i++;
        }
    }
}

void paramSurfaces(BezierPatch G, glm::vec3 res[]){ 
// BezierPatch G(glm::vec3(-1.0f,  1.0f, 0.0f), glm::vec3(-0.5f,  1.0f, 0.0f),
              // glm::vec3( 0.5f,  1.0f, 0.0f), glm::vec3( 1.0f,  1.0f, 0.0f),
              // glm::vec3(-1.0f,  0.5f, 0.0f), glm::vec3(-0.5f,  0.5f, 1.0f),
              // glm::vec3( 0.5f,  0.5f, 1.0f), glm::vec3( 1.0f,  0.5f, 0.0f),
              // glm::vec3(-1.0f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f),
              // glm::vec3( 0.5f, -0.5f, 1.0f), glm::vec3( 1.0f, -0.5f, 0.0f),
              // glm::vec3(-1.0f, -1.0f, 0.0f), glm::vec3(-0.5f, -1.0f, 0.0f),
              // glm::vec3( 0.5f, -1.0f, 0.0f), glm::vec3( 1.0f, -1.0f, 0.0f));
              
glm::vec3 GT[4] = (G[0][0], G[0][1], G[0][2], G[0][3]);

if (flatnessTest(GT)){
    std::cout << "G = " << std::endl;
    std::cout << G << std::endl;

    glm::mat4x4 M(glm::vec4(-1.0f,  3.0f, -3.0f, 1.0f),
                  glm::vec4( 3.0f, -6.0f,  3.0f, 0.0f),
                  glm::vec4(-3.0f,  3.0f,  0.0f, 0.0f),
                  glm::vec4( 1.0f,  0.0f,  0.0f, 0.0f));

    std::cout << "M = " << std::endl;
    std::cout << M << std::endl;

    for (int s = 0.f; s < 5.f; s += 1.f){
        for (int t = 0.f; t < 5.f; t += 1.f){
            glm::vec4 S(s * s * s, s * s, s, 1.0f);
            glm::vec4 T(t * t * t, t * t, t, 1.0f);

            glm::vec3 P = S * glm::transpose(M) * (G * M) * T;
            std::cout << "P(" << s << ", " << t << ") = S^T * M^T * G * M * T = [" << P << "]" << std::endl;

            glm::mat4x4 DLB(glm::vec4(8.0f, 0.0f, 0.0f, 0.0f),
                            glm::vec4(4.0f, 4.0f, 0.0f, 0.0f),
                            glm::vec4(2.0f, 4.0f, 2.0f, 0.0f),
                            glm::vec4(1.0f, 3.0f, 3.0f, 1.0f));
            DLB /= 8.0f;

            glm::mat4x4 DRB(glm::vec4(1.0f, 3.0f, 3.0f, 1.0f),
                            glm::vec4(0.0f, 2.0f, 4.0f, 2.0f),
                            glm::vec4(0.0f, 0.0f, 4.0f, 4.0f),
                            glm::vec4(0.0f, 0.0f, 0.0f, 8.0f));
            DRB /= 8.0f;

            BezierPatch G11 = glm::transpose(DLB) * G * DLB;
            addtolist(G11, res, 0);
            BezierPatch G12 = glm::transpose(DRB) * G * DLB;
            addtolist(G11, res, 15);
            BezierPatch G21 = glm::transpose(DLB) * G * DRB;
            addtolist(G11, res, 31);
            BezierPatch G22 = glm::transpose(DRB) * G * DRB;
            addtolist(G11, res, 47);
            }
        }
    }
}

// void drawBezierSubdivision(glm::vec3 G[], glm::vec3 res[], int i, bool findlength, int j) {
    // // glm::vec3 ft2 = flatnessTest(G[1], G[4], G[2]);
    // // glm::vec3 ft3 = flatnessTest(G[1], G[4], G[3]);
    // if (flatnessTest(G)){
            // // x(flatnessTest(G[0], G[3], G[1]), flatnessTest(G[0], G[3], G[2]))<1.001){
        // glm::vec3 v2 (2.f,2.f,2.f);
        // glm::vec3 v3 (3.f,3.f,3.f);
        // glm::vec3 v4 (4.f,4.f,4.f);
        // glm::vec3 v8 (8.f,8.f,8.f);
        // glm::vec3 left[4];
        // left[0] =  G[0]; 
        // left[1] = (G[0] * v4 + G[1] * v4)/v8;
        // left[2] = (G[0] * v2 + G[1] * v4 + G[2] * v2)/v8;
        // left[3] = (G[0]      + G[1] * v3 + G[2] * v3 + G[3])/v8;

        // glm::vec3 right[4];
        // right[3] =                                 G[3];
        // right[2] =                    (G[2] * v4 + G[3] * v4)/v8;
        // right[1] =        (G[1] * v2 + G[2] * v4 + G[3] * v2)/v8;
        // right[0] = (G[0] + G[1] * v3 + G[2] * v3 + G[3]     )/v8;

        // // std::cout << i << "  " << findlength << std::endl;
        // if (not findlength)
        // {
            // drawBezierSubdivision(left, res, floor(i/2), findlength, j);    
            // res[j] = left[3]; // save values 
            // j =+ 3;
            // // drawLine(POld, Left[4]); // Draws the middle line of the current curve
            // // POld = left[3];
            // drawBezierSubdivision(right, res, ceil(i/2), findlength, j); // Calculates and draws the right side
        // }
        // else
        // {
            // j = j + 1;
            // drawBezierSubdivision(left, res, floor(i/2), findlength, j); // Calculates and draws the left side by creating a subcurve
            // drawBezierSubdivision(right, res, ceil(i/2), findlength, j); // Calculates and draws the right side
            // // std::cout << "j = " << j << std::endl;
        // }
        // // i += 1; // increment 1 

    // }
// }

// int findlength(glm::vec3 G[]){
    // int j = 0;
    // glm::vec3 res[0];
    // drawBezierSubdivision(G, res, 0, true, j);
    // std::cout << "j = " << j << std::endl;
    // return j;
// }

void GLCurve::initializeBuffers(ShaderProgram & shaderProgram, glm::vec3 G[]) {
    // drawBezierSubdivision(G, curve, i, false); 
    // findlength(G);

    float curve[9] = { 
        -33.978017f, -34.985076f,  50.214926f,
         84.192943f, -13.784394f, -50.214926f,
        -16.236910f,  83.754546f, -50.214926f
    };
    
	// m_numberOfCurves = 3;
    m_numberOfCurves  = sizeof(curve) / sizeof(float); 
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");

	//Generate a vertex buffer object (m_VBO), and copy the array data (vertices) to it:
	glGenBuffers(1, &m_Curve);
	glBindBuffer(GL_ARRAY_BUFFER, m_Curve);
	glBufferData(GL_ARRAY_BUFFER, sizeof(curve), curve, GL_STATIC_DRAW);

	//Get a handle to the shader attribute aPosition and set it up:
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}

