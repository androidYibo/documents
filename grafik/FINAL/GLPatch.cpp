/*
	Author: Kasper Passov 
	Date: 17-Feb-2014
*/  

#include <iostream>

#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "bezierpatch.h"

#include "GLPatch.h"

void GLPatch::addtolist(BezierPatch G, float res[], int i)
{
    for(int row = 0; row < 4; row++){
        for(int vec = 0; vec < 4; vec++){
            for(int flo = 0; flo < 3; flo++){
                res[i] = G[row][vec][flo];
                i++;
            }
        }
    }
}

void GLPatch::paramSurfaces(BezierPatch G, float res[])
{ 

glm::mat4x4 M(glm::vec4(-1.0f,  3.0f, -3.0f, 1.0f),
              glm::vec4( 3.0f, -6.0f,  3.0f, 0.0f),
              glm::vec4(-3.0f,  3.0f,  0.0f, 0.0f),
              glm::vec4( 1.0f,  0.0f,  0.0f, 0.0f));

for (float s = 0.f; s < 5.f; s += 1.f){
    for (float t = 0.f; t < 5.f; t += 1.f){
        glm::vec4 S(s * s * s, s * s, s, 1.0f);
        glm::vec4 T(t * t * t, t * t, t, 1.0f);

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
        addtolist(G11, res, 47);
        BezierPatch G21 = glm::transpose(DLB) * G * DRB;
        addtolist(G11, res, 95);
        BezierPatch G22 = glm::transpose(DRB) * G * DRB;
        addtolist(G11, res, 143);
        }
    }
}


void GLPatch::draw() {
	glEnableVertexAttribArray(m_vertexPositionAttribute);
    
    for (int i = 0; i < 32; i++){
        glBindBuffer(GL_ARRAY_BUFFER, m_Patches[i]);
        glDrawArrays(GL_POINTS, 0, m_numberOfPatches[i]);
    }

    glDisableVertexAttribArray(m_vertexPositionAttribute);
}

void GLPatch::initializeBuffers(ShaderProgram & shaderProgram) {

    std::vector<BezierPatch> patchesv;

    ReadBezierPatches("data/teapot.data", patchesv);
    
    float patchesf[patchesv.size()][192];
    for (int i = 0; i < patchesv.size(); i++)
    {
       float patch[192];
       paramSurfaces(patchesv[i], patch); 
       m_numberOfPatches[i] = sizeof(patch) / sizeof(float); //i know this will be constant

       m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");

       GLuint m_Patch;
       m_Patches[i] = m_Patch;
       glGenBuffers(1, &m_Patch);
       glBindBuffer(GL_ARRAY_BUFFER, m_Patch);
       glBufferData(GL_ARRAY_BUFFER, sizeof(patch), patch, GL_STATIC_DRAW);
    }

	// glGenBuffers(1, &m_Patch);
	// glBindBuffer(GL_ARRAY_BUFFER, m_Patch);
	// glBufferData(GL_ARRAY_BUFFER, sizeof(patch), patch, GL_STATIC_DRAW);
    
    glm::vec3 Normal = glm::cross((patchesv[0][0][0] - patchesv[0][0][2]),(patchesv[0][0][2] - patchesv[0][3][3]));
    glm::vec3 N = glm::normalize(glm::vec3(Normal[0]+0.00001f,Normal[1]+0.000001f,Normal[2]+0.00001f));
    glUniform4f(glGetUniformLocation(shaderProgram.getProgram(), "uNormal"), N[0], N[0], N[2], 1.0);

	//Get a handle to the shader attribute aPosition and set it up:
	m_vertexPositionAttribute = glGetAttribLocation(shaderProgram.getProgram(), "aPosition");
	glVertexAttribPointer(m_vertexPositionAttribute, 3, GL_FLOAT, GL_FALSE, 0, 0);
}
