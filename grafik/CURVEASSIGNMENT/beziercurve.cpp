#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include "glmutils.h"
#include "beziercurve.h"


/**
 * Default constructor creates an empty BezierVec4, i.e. all entries are the zero vector.
 */
BezierVec4::BezierVec4()
{
    glm::vec3 zero(0.0f, 0.0f, 0.0f);
    for (int i = 0; i < 4; ++i) this->controlpoints[i] = zero;
}

/**
 * Parameterized constructor creates a BezierVec4 containing the arguments as entries.
 * \param G1 - Entry one in the geometry vector.
 * \param G2 - Entry two in the geometry vector.
 * \param G3 - Entry three in the geometry vector.
 * \param G4 - Entry four in the geometry vector.
 */
BezierVec4::BezierVec4(glm::vec3 const& G1, glm::vec3 const& G2, glm::vec3 const& G3, glm::vec3 const& G4)
{
    this->controlpoints[0] = G1;
    this->controlpoints[1] = G2;
    this->controlpoints[2] = G3;
    this->controlpoints[3] = G4;
}

/**
 * Copy constructor creates a new BezierVec4 which is a copy of its argument.
 * \param geometryvector - The geometry vector to be copied.
 */
BezierVec4::BezierVec4(BezierVec4 const& geometryvector)
{
    for (int i = 0; i < 4; ++i) this->controlpoints[i] = geometryvector.controlpoints[i];
}

/**
 * Destructor destroys the current instance of BezierVec4.
 */
BezierVec4::~BezierVec4()
{}

/**
 * Assignent operator assigns its argument to the current instance of BezierVec4.
 * \lparam geometryvector - The geometry vector to be assigned to this instance.
 */
BezierVec4& BezierVec4::operator=(BezierVec4 const& geometryvector)
{
    if (this != &geometryvector) {
	for (int i = 0; i < 4; ++i) this->controlpoints[i] = geometryvector.controlpoints[i];
    }
    return *this;
}

/**
 * Index operator - read only - returns the i'th entry in the geometry vector,
 * The entry is returned as a homogeneous vector.
 * \param i - The index of the entry to be returned as a homogeneous vector.
 */
glm::vec3 const& BezierVec4::operator[](int i) const
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierVec4::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlpoints[i - 1];
}

/**
     * Index operator returns a reference to the i'th entry in the geometry vector.
     * The reference is to a homogeneous vector, and it can be assigned to (read/write).
     * \param i - The index of the entry to which a reference is to be returned;
     */
glm::vec3& BezierVec4::operator[](int i)
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierVec4::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlpoints[i - 1];
}


/**
 * Utlity Functions
 */

/**
 * Multiplication operator, right-multiplies a geometry vector by an ordinary vector (a parameter vector).
 * This can be used to right-multiply the Bezier matrices by the parameter vector. 
 * \param geometryvector - The BezierVec4 that should be multimplied.
 * \param vector - The vector (a parameter vector) that is right-multiplied by the geometry vector.
 * \return The product geometryvector * vector.
 */
glm::vec3 operator*(BezierVec4 const& geometryvector, glm::vec4 const& vector)
{
    glm::vec3 result(0.0f);
    
    for (int i = 0; i < 4; ++i) {
	result += geometryvector[i + 1] * vector[i];
    }
    return result;
}

/**
 * Multiplication operator, right-multiplies a geometry vector by an ordinary matrix (a basis matrix).
 * This can be used to right-multiply a Bezier geometry vector by an ordinary matrix (a basis matrix).
 * \param geometryvector - The BezierVec4 that should be multiplied.
 * \param matrix - The ordinary matrix to be right-multiplied (basis matrix) by the geometry vector.
 * \return The product geometryvector * matrix.
 */
BezierVec4 operator*(BezierVec4 const& geometryvector, glm::mat4x4 const& matrix)
{
    glm::vec3 zeroes(0.0f);
    BezierVec4 result(zeroes, zeroes, zeroes, zeroes);

    for (int c = 0; c < 4; c++) {
	glm::vec4 column(glm::column(matrix, c));
	for (int i = 0; i < 4; ++i) {
	    result[c + 1] += geometryvector[i + 1] * column[i];  
	}
    }
    return result;
}

/**
 * Multiplication operator, left-multiplies an ordinary matrix (a transformation matrix) by a geometry vector.
 * It multiplies the control points by the matrix thereby transforming them.
 * \param matrix - The ordinary matrix (transformation matrix) to be left-multiplied by the geometry vector.
 * \param geometryvector - The BezierVec4 that should be multiplied.
 * \return A new BezierVec4 with the element[i] = matrix * geometryvector[i]; i = 1,...,4
 */
BezierVec4 operator*(glm::mat4x4 const& matrix, BezierVec4 const& geometryvector)
{
    BezierVec4 result;

    for (int i = 1; i <= 4; ++i) {
	glm::vec4 tmpvec = matrix * glm::vec4(geometryvector[i], 1.0f);
	if (tmpvec.w == 0.0) {
	    throw std::runtime_error("operator*(glm::mat4x4&, BezierVec4&): Division by zero");
	}
	result[i] = glm::vec3(tmpvec.x / tmpvec.w, tmpvec.y / tmpvec.w, tmpvec.z / tmpvec.w); 
    }
    return result;
}

/**
 * Insertion operator, inserts a BezierVec4 into an ostream.
 * \param s - The ostream which the geometryvector should be inserted into.
 * \param geometryvector - The BezierVec4 that should be inserted into the ostream.
 * \return The ostream which the geometryvector has been inserted into.
 */
std::ostream& operator<<(std::ostream& s, BezierVec4 const& geometryvector)
{
    s << ' ';
    for (int i = 1; i < 4; ++i) {
	s << std::setw(6) << std::setprecision(4) << geometryvector[i] << " | ";
    }
    s << std::setw(6) << std::setprecision(4) << geometryvector[4] << ' ';

    return s;
}

