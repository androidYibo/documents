#ifndef __BEZIER_CURVE_H__
#define __BEZIER_CURVE_H__

#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include "glm/glm.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "glm/gtx/transform.hpp"
#include "glmutils.h"


/**
 * \class BezierVec4 implements the concept of a geometry vector for a parametric curve where each 
 * coordinate function is a polynomial of degree 3.
 */
class BezierVec4 {
public:
    /**
     * Default constructor creates an empty BezierVec4, i.e. all entries are the zero vector.
     */
    BezierVec4();

    /**
     * Parameterized constructor creates a BezierVec4 containing the arguments as entries.
     * \param G1 - Entry one in the geometry vector.
     * \param G2 - Entry two in the geometry vector.
     * \param G3 - Entry three in the geometry vector.
     * \param G4 - Entry four in the geometry vector.
     */
    BezierVec4(glm::vec3 const& G1, glm::vec3 const& G2, glm::vec3 const& G3, glm::vec3 const& G4);

    /**
     * Copy constructor creates a new BezierVec4 which is a copy of its argument.
     * \param geometryvector - The geometry vector to be copied.
     */
    BezierVec4(BezierVec4 const& geometryvector);

    /**
     * Destructor destroys the current instance of BezierVec4.
     */
    virtual ~BezierVec4();

    /**
     * Assignent operator assigns its argument to the current instance of BezierVec4.
     * \lparam geometryvector - The geometry vector to be assigned to this instance.
     */
    BezierVec4& operator=(BezierVec4 const& geomtryvector);

    /**
     * Index operator - read only - returns the i'th entry in the geometry vector,
     * The entry is returned as a homogeneous vector.
     * \param i - The index of the entry to be returned as a homogeneous vector.
     */
    glm::vec3 const& operator[](int i) const;

    /**
     * Index operator returns a reference to the i'th entry in the geometry vector.
     * The reference is to a homogeneous vector, and it can be assigned to (read/write).
     * \param i - The index of the entry to which a reference is to be returned;
     */
    glm::vec3& operator[](int i);

protected:

private:
    /**
     * The four Control Points of a Bezier Curve
     */
    glm::vec3 controlpoints[4];
};


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
glm::vec3 operator*(BezierVec4 const& geometryvector, glm::vec4 const& vector);

/**
 * Multiplication operator, right-multiplies a geometry vector by an ordinary matrix (a basis matrix).
 * This can be used to right-multiply a Bezier geometry vector by an ordinary matrix (a basis matrix).
 * \param geometryvector - The BezierVec4 that should be multiplied.
 * \param matrix - The ordinary matrix to be right-multiplied (basis matrix) by the geometry vector.
 * \return The product geometryvector * matrix.
 */
BezierVec4 operator*(BezierVec4 const& geometryvector, glm::mat4x4 const& matrix);

/**
 * Multiplication operator, left-multiplies an ordinary matrix (a transformation matrix) by a geometry vector.
 * It multiplies the control points by the matrix thereby transforming them.
 * \param matrix - The ordinary matrix (transformation matrix) to be left-multiplied by the geometry vector.
 * \param geometryvector - The BezierVec4 that should be multiplied.
 * \return A new BezierVec4 with the element[i] = matrix * geometryvector[i]; i = 1,...,4
 */
BezierVec4 operator*(glm::mat4x4 const& matrix, BezierVec4 const& geometryvector);

/**
 * Insertion operator, inserts a BezierVec4 into an ostream.
 * \param s - The ostream which the geometryvector should be inserted into.
 * \param geometryvector - The BezierVec4 that should be inserted into the ostream.
 * \return The ostream which the geometryvector has been inserted into.
 */
std::ostream& operator<<(std::ostream& s, BezierVec4 const& geometryvector);

#endif
