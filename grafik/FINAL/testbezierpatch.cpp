#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include "glm/glm.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "glmutils.h"

#include "bezierpatch.h"


void BP_ParameterizedConstructor()
{
    std::cout << "-->BP_ParameterizedConstructor()" << std::endl;

    BezierPatch G(glm::vec3(-1.0f,  1.0f, 0.0f), glm::vec3(-0.5f,  1.0f, 0.0f),
		  glm::vec3( 0.5f,  1.0f, 0.0f), glm::vec3( 1.0f,  1.0f, 0.0f),
		  glm::vec3(-1.0f,  0.5f, 0.0f), glm::vec3(-0.5f,  0.5f, 1.0f),
		  glm::vec3( 0.5f,  0.5f, 1.0f), glm::vec3( 1.0f,  0.5f, 0.0f),
		  glm::vec3(-1.0f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f),
		  glm::vec3( 0.5f, -0.5f, 1.0f), glm::vec3( 1.0f, -0.5f, 0.0f),
		  glm::vec3(-1.0f, -1.0f, 0.0f), glm::vec3(-0.5f, -1.0f, 0.0f),
		  glm::vec3( 0.5f, -1.0f, 0.0f), glm::vec3( 1.0f, -1.0f, 0.0f));
    std::cout << "G = " << std::endl;
    std::cout << G << std::endl;

    std::cout << "BezierPatch G1(G);" << std::endl;
    BezierPatch G1(G);

    std::cout << "<--BP_ParameterizedConstructor()" << std::endl;
}

void BP_IndexOperatorConst()
{
    std::cout << "-->BP_IndexOperatorConst()" << std::endl;

    BezierPatch G(glm::vec3(-1.0f,  1.0f, 0.0f), glm::vec3(-0.5f,  1.0f, 0.0f),
		  glm::vec3( 0.5f,  1.0f, 0.0f), glm::vec3( 1.0f,  1.0f, 0.0f),
		  glm::vec3(-1.0f,  0.5f, 0.0f), glm::vec3(-0.5f,  0.5f, 1.0f),
		  glm::vec3( 0.5f,  0.5f, 1.0f), glm::vec3( 1.0f,  0.5f, 0.0f),
		  glm::vec3(-1.0f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f),
		  glm::vec3( 0.5f, -0.5f, 1.0f), glm::vec3( 1.0f, -0.5f, 0.0f),
		  glm::vec3(-1.0f, -1.0f, 0.0f), glm::vec3(-0.5f, -1.0f, 0.0f),
		  glm::vec3( 0.5f, -1.0f, 0.0f), glm::vec3( 1.0f, -1.0f, 0.0f));
    std::cout << "G = " << std::endl;
    std::cout << G << std::endl;

    for (int i = 1; i <= 4; ++i) {
	for (int j = 1; j <= 4; ++j) {
	    std::cout << "G[" << i << "][" << j << "] = " 
		      << std::setw(6) << std::setprecision(4) << G[i][j] << std::endl;
	}
    }

    std::cout << "<--BP_IndexOperatorConst()" << std::endl;
}

void BP_IndexOperator()
{
    std::cout << "-->BP_IndexOperator()" << std::endl;

    BezierPatch G(glm::vec3(-1.0f,  1.0f, 0.0f), glm::vec3(-0.5f,  1.0f, 0.0f),
		  glm::vec3( 0.5f,  1.0f, 0.0f), glm::vec3( 1.0f,  1.0f, 0.0f),
		  glm::vec3(-1.0f,  0.5f, 0.0f), glm::vec3(-0.5f,  0.5f, 1.0f),
		  glm::vec3( 0.5f,  0.5f, 1.0f), glm::vec3( 1.0f,  0.5f, 0.0f),
		  glm::vec3(-1.0f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f),
		  glm::vec3( 0.5f, -0.5f, 1.0f), glm::vec3( 1.0f, -0.5f, 0.0f),
		  glm::vec3(-1.0f, -1.0f, 0.0f), glm::vec3(-0.5f, -1.0f, 0.0f),
		  glm::vec3( 0.5f, -1.0f, 0.0f), glm::vec3( 1.0f, -1.0f, 0.0f));
    std::cout << "G = " << std::endl;
    std::cout << G << std::endl;

    G[1][1] = glm::vec3(1.0f, 1.0f, 0.0f);
    G[1][2] = glm::vec3(1.0f, 2.0f, 0.0f);
    G[1][3] = glm::vec3(1.0f, 3.0f, 0.0f);
    G[1][4] = glm::vec3(1.0f, 4.0f, 0.0f);
    G[2][1] = glm::vec3(2.0f, 1.0f, 0.0f);
    G[2][2] = glm::vec3(2.0f, 2.0f, 0.0f);
    G[2][3] = glm::vec3(2.0f, 3.0f, 0.0f);
    G[2][4] = glm::vec3(2.0f, 4.0f, 0.0f);
    G[3][1] = glm::vec3(3.0f, 1.0f, 0.0f);
    G[3][2] = glm::vec3(3.0f, 2.0f, 0.0f);
    G[3][3] = glm::vec3(3.0f, 3.0f, 0.0f);
    G[3][4] = glm::vec3(3.0f, 4.0f, 0.0f);
    G[4][1] = glm::vec3(4.0f, 1.0f, 0.0f);
    G[4][2] = glm::vec3(4.0f, 2.0f, 0.0f);
    G[4][3] = glm::vec3(4.0f, 3.0f, 0.0f);
    G[4][4] = glm::vec3(4.0f, 4.0f, 0.0f);

    std::cout << "G = " << std::endl;
    std::cout << G << std::endl;

    std::cout << "<--BP_IndexOperator()" << std::endl;
}

void BP_RightMatMult()
{
    std::cout << "-->BP_RightMatMult()" << std::endl;

    BezierPatch G(glm::vec3(-1.0f,  1.0f, 0.0f), glm::vec3(-0.5f,  1.0f, 0.0f),
		  glm::vec3( 0.5f,  1.0f, 0.0f), glm::vec3( 1.0f,  1.0f, 0.0f),
		  glm::vec3(-1.0f,  0.5f, 0.0f), glm::vec3(-0.5f,  0.5f, 1.0f),
		  glm::vec3( 0.5f,  0.5f, 1.0f), glm::vec3( 1.0f,  0.5f, 0.0f),
		  glm::vec3(-1.0f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f),
		  glm::vec3( 0.5f, -0.5f, 1.0f), glm::vec3( 1.0f, -0.5f, 0.0f),
		  glm::vec3(-1.0f, -1.0f, 0.0f), glm::vec3(-0.5f, -1.0f, 0.0f),
		  glm::vec3( 0.5f, -1.0f, 0.0f), glm::vec3( 1.0f, -1.0f, 0.0f));
    std::cout << "G = " << std::endl;
    std::cout << G << std::endl;

    glm::mat4x4 M(glm::vec4(-1.0f,  3.0f, -3.0f, 1.0f),
		  glm::vec4( 3.0f, -6.0f,  3.0f, 0.0f),
		  glm::vec4(-3.0f,  3.0f,  0.0f, 0.0f),
		  glm::vec4( 1.0f,  0.0f,  0.0f, 0.0f));

    std::cout << "M = " << std::endl;
    std::cout << M << std::endl;

    BezierPatch GM = G * M;    
    std::cout << "G * M =" << std::endl;
    std::cout << GM << std::endl;

    std::cout << "M^T = " << std::endl;
    std::cout << glm::transpose(M) << std::endl;

    BezierPatch MTG = glm::transpose(M) * G;
    std::cout << "M^T * G =" << std::endl;
    std::cout << MTG << std::endl;

    BezierPatch MTGM = glm::transpose(M) * G * M;
    std::cout << "M^T * G * M =" << std::endl;
    std::cout << MTGM << std::endl;

    float s = 0.0f;
    float t = 0.0f;

    glm::vec4 S(s * s * s, s * s, s, 1.0f);
    glm::vec4 T(t * t * t, t * t, t, 1.0f);

    glm::vec3 P = S * MTGM * T;
    std::cout << "P(" << s << ", " << t << ") = S^T * M^T * G * M * T = ["
	      << P << "]" << std::endl;

    s = 1.0f;
    S = glm::vec4(s * s * s, s * s, s, 1.0f);

    P = S * MTGM * T;
    std::cout << "P(" << s << ", " << t << ") = S^T * M^T * G * M * T = ["
	      << P << "]" << std::endl;

    t = 1.0f;
    T = glm::vec4(t * t * t, t * t, t, 1.0f);

    P = S * MTGM * T;
    std::cout << "P(" << s << ", " << t << ") = S^T * M^T * G * M * T = ["
	      << P << "]" << std::endl;

    s = 0.0f;
    S = glm::vec4(s * s * s, s * s, s, 1.0f);

    P = S * MTGM * T;
    std::cout << "P(" << s << ", " << t << ") = S^T * M^T * G * M * T = ["
	      << P << "]" << std::endl;

    std::cout << std::endl;
    std::cout << "<--BP_RightMatMult()" << std::endl;
}


int main()
{
    try {
	BP_ParameterizedConstructor();
	BP_IndexOperatorConst();
	BP_IndexOperator();
	BP_RightMatMult();
    }
    catch (std::exception const& e) {
	std::cout << "Exception: " << e.what() << std::endl;
    }
    return 0;
}
