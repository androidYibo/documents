/*
	Author: Mads Jeppe Lyngholm Rønnow
	Date: 17-Feb-2014
*/

#pragma once

#include <vector>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include "ShaderProgram.h"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "bezierpatch.h"
#include "readbezierpatches.h"

class GLPatch
{
public:
	GLPatch() {}

	void draw();
	void initializeBuffers(ShaderProgram & shaderProgram);

	~GLPatch() {}

private: 
    void addtolist(BezierPatch, float[], int);
    void paramSurfaces(BezierPatch, float[]);
	GLuint m_Patches[];
	int m_numberOfPatches[];
    std::vector<BezierPatch> patchesv;

	GLuint m_vertexPositionAttribute;
};

