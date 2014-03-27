#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include "glm/glm.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "glmutils.h"

#include "beziercurve.h"


void DefaultConstructor()
{
    std::cout << "-->BezierVec4()" << std::endl;

    std::cout << "   Test of Default Constructor" << std::endl;
    std::cout << "   ===========================" << std::endl;

    BezierVec4 G;

    std::cout << "G = " << std::endl << G << std::endl;

    std::cout << "<--BezierVec4()" << std::endl;
    std::cout << std::endl;
}

void ParameterizedConstructorVec3()
{
    std::cout << "-->BezierVec4(4 x glm::vec3 const&)" << std::endl;

    std::cout << "   Test of Parameterized Constructor" << std::endl;
    std::cout << "   =================================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    BezierVec4 G(G1, G2, G3, G4);
    std::cout << "G = " << std::endl << G << std::endl;

    std::cout << "<--BezierVec4(4 x glm::vec3 const&)" << std::endl;
    std::cout << std::endl;
}

void CopyConstructor()
{
    std::cout << "-->BezierVec4(BezierVec4 const&)" << std::endl;

    std::cout << "   Test of Copy Constructor" << std::endl;
    std::cout << "   ========================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);
    
    BezierVec4 B1(G1, G2, G3, G4);
    std::cout << "B1 = " << std::endl << B1 << std::endl;

    BezierVec4 B2(B1);
    std::cout << "B2(B1) = " << std::endl << B2 << std::endl;

    std::cout << "<--BezierVec4(BezierVec4 const&)" << std::endl;
    std::cout << std::endl;
}

void AssignmentOperator()
{
    std::cout << "-->operator=(BezierVec4 const&)" << std::endl;

    std::cout << "   Test of Assignment Operator" << std::endl;
    std::cout << "   ===========================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    BezierVec4 B1(G1, G2, G3, G4);
    std::cout << "B1 = " << std::endl << B1 << std::endl;

    glm::vec3 G5(0.0f, 0.0f, 1.0f);
    glm::vec3 G6(0.0f, 1.0f, 0.0f);
    glm::vec3 G7(1.0f, 0.0f, 0.0f);
    glm::vec3 G8(1.0f, 1.0f, 1.0f);

    BezierVec4 B2(G5, G6, G7, G8);
    std::cout << "B2 = " << std::endl << B2 << std::endl;

    std::cout << "B1 = B2" << std::endl;
    B1 = B2;
    std::cout << "B1 = " << std::endl << B1 << std::endl;
    std::cout << "B2 = " << std::endl << B2 << std::endl;

    std::cout << "<--operator=(BezierVec4 const&)" << std::endl;
    std::cout << std::endl;
}

void IndexOperatorConst()
{
    std::cout << "-->operator[](int) --- const" << std::endl;

    std::cout << "   Test of Index Operator - const" << std::endl;
    std::cout << "   ==============================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    BezierVec4 B(G1, G2, G3, G4);
    std::cout << "B = " << std::endl << B << std::endl;
    for (int i = 1; i <= 4; ++i) {
	std::cout << "B[" << i << "] = " << B[i] << std::endl;
    }

    std::cout << "<--operator[](int) --- const" << std::endl;
    std::cout << std::endl;
}

void IndexOperator()
{
    std::cout << "-->operator[](int)" << std::endl;

    std::cout << "   Test of Index Operator" << std::endl;
    std::cout << "   ======================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    std::cout << "G1 = " << G1 << std::endl;
    std::cout << "G2 = " << G2 << std::endl;
    std::cout << "G3 = " << G3 << std::endl;
    std::cout << "G4 = " << G4 << std::endl;

    BezierVec4 B;
    std::cout << "B = " << std::endl << B << std::endl;
    
    std::cout << "B[1] = G1" << std::endl;
    B[1] = G1;
    std::cout << "B[1] = " << B[1] << std::endl;

    std::cout << "B[2] = G2" << std::endl;
    B[2] = G2;
    std::cout << "B[2] = " << B[2] << std::endl;

    std::cout << "B[3] = G3" << std::endl;
    B[3] = G3;
    std::cout << "B[3] = " << B[3] << std::endl;

    std::cout << "B[4] = G4" << std::endl;
    B[4] = G4;
    std::cout << "B[4] = " << B[4] << std::endl;

    std::cout << "<--operator[](int)" << std::endl;
    std::cout << std::endl;
}

void MultGeomvecVec()
{
    std::cout << "-->operator*(BezierVec4&, glm::vec4&)" << std::endl;

    std::cout << "   Test of Operator* (BezierVec4 * vec4)" << std::endl;
    std::cout << "   =====================================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    std::cout << "G1 = " << G1 << std::endl;
    std::cout << "G2 = " << G2 << std::endl;
    std::cout << "G3 = " << G3 << std::endl;
    std::cout << "G4 = " << G4 << std::endl;

    BezierVec4 B(G1, G2, G3, G4);
    std::cout << "B = " << B << std::endl;

    glm::vec4 T(1.0f, 2.0f, 3.0f, 4.0f);
    std::cout << "T = " << T << std::endl;

    glm::vec3 BT = B * T;
    std::cout << "B * T = " << BT << std::endl;

    std::cout << "<--operator*(BezierVec4&, glm::vec4&)" << std::endl;
    std::cout << std::endl;
}

void MultGeomvecMat()
{
    std::cout << "-->operator*(BezierVec4&, glm::mat4x4&)" << std::endl;

    std::cout << "   Test of Operator* (BezierVec4 * mat4x4)" << std::endl;
    std::cout << "   =======================================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    std::cout << "G1 = " << G1 << std::endl;
    std::cout << "G2 = " << G2 << std::endl;
    std::cout << "G3 = " << G3 << std::endl;
    std::cout << "G4 = " << G4 << std::endl;

    BezierVec4 B(G1, G2, G3, G4);
    std::cout << "B = " << B << std::endl;

    glm::mat4x4 M = glm::scale(2.0f, 3.0f, 4.0f);
    std::cout << "M = " << std::endl;
    std::cout << M << std::endl;

    BezierVec4 BM = B * M;
    std::cout << "B * M = " << BM << std::endl;

    std::cout << "<--operator*(BezierVec4&, glm::mat4x4&)" << std::endl;
    std::cout << std::endl;
}

void MultMatGeomvec()
{
    std::cout << "-->operator*(glm::mat4x4&, BezierVec4&)" << std::endl;

    std::cout << "   Test of Operator* (mat4x4 * BezierVec4)" << std::endl;
    std::cout << "   =======================================" << std::endl;

    glm::vec3 G1(1.0f, 0.0f, 0.0f);
    glm::vec3 G2(0.0f, 1.0f, 0.0f);
    glm::vec3 G3(0.0f, 0.0f, 1.0f);
    glm::vec3 G4(1.0f, 1.0f, 1.0f);

    std::cout << "G1 = " << G1 << std::endl;
    std::cout << "G2 = " << G2 << std::endl;
    std::cout << "G3 = " << G3 << std::endl;
    std::cout << "G4 = " << G4 << std::endl;

    BezierVec4 B(G1, G2, G3, G4);
    std::cout << "B = " << B << std::endl;

    glm::mat4x4 M = glm::scale(2.0f, 3.0f, 4.0f);
    std::cout << "M = " << std::endl;
    std::cout << M << std::endl;

    BezierVec4 MB = M * B;
    std::cout << "M * B = " << MB << std::endl;

    std::cout << "<--operator*(glm::mat4x4&, BezierVec4&)" << std::endl;
    std::cout << std::endl;
}


int main()
{
    try {
	DefaultConstructor();
	ParameterizedConstructorVec3();
	CopyConstructor();
	AssignmentOperator();
	IndexOperatorConst();
	IndexOperator();
	MultGeomvecVec();
	MultGeomvecMat();
	MultMatGeomvec();
    }
    catch (std::exception& e) {
	std::cout << e.what() << std::endl;
    }
    return 0;
}
