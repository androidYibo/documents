#ifndef __BEZIERPATCH_H__
#define __BEZIERPATCH_H__

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
 * \class BezierRow 
 * implements the concept of a row of a geometry matrix for a parametric surface where
 * each coordinate function is a polynomial of degree 3.
 */
class BezierRow {
public:
    /**
     * Default constructor creates an empty BezierRow, i.e. all entries are the zero vector.
     */
    BezierRow();

    /**
     * Parameterized constructor creates a BezierRow containing the arguments as entries.
     * \param G1 - Entry one in the geometry row vector.
     * \param G2 - Entry two in the geometry row vector.
     * \param G3 - Entry three in the geometry row vector.
     * \param G4 - Entry four in the geometry row vector.
     */
    BezierRow(glm::vec3 const& G1, glm::vec3 const& G2, glm::vec3 const& G3, glm::vec3 const& G4);

    /**
     * Copy constructor creates a new BezierRow which is a copy of its argument.
     * \param bezierrow - The geometry row vector to be copied.
     */
    BezierRow(BezierRow const& bezierrow);

    /**
     * Destructor destroys the current instance of BezierRow.
     */
    virtual ~BezierRow();

    /**
     * Assignent operator assigns its argument to the current instance of BezierRow.
     * \lparam bezierrow - The geometry row vector to be assigned to this instance.
     */
    BezierRow& operator=(BezierRow const& bezierrow);

    /**
     * Index operator - read only - returns the i'th entry in the geometry row vector,
     * \param i - The index of the entry to be returned.
     */
    glm::vec3 const& operator[](int i) const;

    /**
     * Index operator returns a reference to the i'th entry in the geometry row vector.
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
 * \class BezierColumn
 * implements the concept of a column of a geometry matrix for a parametric surface where
 * each coordinate function is a polynomial of degree 3.
 */
class BezierColumn {
public:
    /**
     * Default constructor creates an empty BezierColumn, i.e. all entries are the zero vector.
     */
    BezierColumn();

    /**
     * Parameterized constructor creates a BezierColumn containing the arguments as entries.
     * \param G1 - Entry one in the geometry column vector.
     * \param G2 - Entry two in the geometry column vector.
     * \param G3 - Entry three in the geometry column vector.
     * \param G4 - Entry four in the geometry column vector.
     */
    BezierColumn(glm::vec3 const& G1, glm::vec3 const& G2, glm::vec3 const& G3, glm::vec3 const& G4);

    /**
     * Copy constructor creates a new BezierColumn which is a copy of its argument.
     * \param beziercolumn - The geometry column vector to be copied.
     */
    BezierColumn(BezierColumn const& beziercolumn);

    /**
     * Destructor destroys the current instance of BezierColumn.
     */
    virtual ~BezierColumn();

    /**
     * Assignent operator assigns its argument to the current instance of BezierColumn.
     * \lparam beziercolum - The geometry column vector to be assigned to this instance.
     */
    BezierColumn& operator=(BezierColumn const& beziercolumn);

    /**
     * Index operator - read only - returns the i'th entry in the geometry column vector,
     * \param i - The index of the entry to be returned.
     */
    glm::vec3 const& operator[](int i) const;

    /**
     * Index operator returns a reference to the i'th entry in the geometry column vector.
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
 * \class BezierPacth 
 *implements the concept of a geometry matrix for a parametric surface where each 
 * coordinate function is a polynomial of degree 3.
 */
class BezierPatch {
public:
    /**
     * Default constructor creates an empty BezierPatch, i.e. all entries are the zero vector.
     */
    BezierPatch();

    /**
     * Parameterized constructor creates a BezierPatch containing the arguments as entries.
     * \param G11,..., G44 - The entries of the geometry matrix.
     */
    BezierPatch(glm::vec3 g11, glm::vec3 g12, glm::vec3 g13, glm::vec3 g14,
	        glm::vec3 g21, glm::vec3 g22, glm::vec3 g23, glm::vec3 g24,
	        glm::vec3 g31, glm::vec3 g32, glm::vec3 g33, glm::vec3 g34,
	        glm::vec3 g41, glm::vec3 g42, glm::vec3 g43, glm::vec3 g44);

    /**
     * Copy constructor creates a new BezierPatch which is a copy of its argument.
     * \param bezierpatch - The bezierpatch to be copied.
     */
    BezierPatch(BezierPatch const& bezierpatch);

    /**
     * Destructor destroys the current instance of BezierPatch.
     */
    virtual ~BezierPatch();

    /**
     * Assignent operator assigns its argument to the current instance of BezierPatch.
     * \lparam bezierpatch - The bezierpatch to be assigned to this instance.
     */
    BezierPatch& operator=(BezierPatch const& bezierpatch);

    /**
     * Index operator - read only - returns the i'th row of the geometry matrix,
     * \param i - The index of the row to be returned.
     */
    BezierRow const& operator[](int i) const;

    /**
     * Index operator returns a reference to the i'th row iof the geometry matrix.
     * \param i - The index of the row to which a reference is to be returned;
     */
    BezierRow& operator[](int i);

protected:

private:
    /**
     * The 16 Control Points of a Bezier Curve
     */
    BezierRow controlvec[4];
};


/**
 * Utility Operators
 */

/**
 * Multiplication operator, right-multiplies a BezierPatch by an ordinary matrix (a basis matrix).
 * This can be used to right-multiply a Bezier geometry matrix by an ordinary matrix (a basis matrix).
 * \param bezierpatch - The BezierPatch that should be multiplied.
 * \param matrix - The ordinary matrix to be right-multiplied (basis matrix) by the bezier patch.
 * \return The product bezierpatch * matrix which is of type BezierPatch.
 */
BezierPatch operator*(BezierPatch const& bezierpatch, glm::mat4x4 const& matrix);

/**
 * Multiplication operator, left-multiplies a BezierPatch by an ordinary matrix (a basis matrix).
 * This can be used to left-multiply a BezierPatch by a basis matrix. 
 * \param bezierpatch - The BezierPatch that should be multimplied.
 * \param matrix - The matrix (a basis matrix) that is left-multiplied by the bezier patch.
 * \return The product matrix * bezierpatch which is of type BezierPatch.
 */
BezierPatch operator*(glm::mat4x4 const& matrix, BezierPatch const& bezierpatch);

/**
 * Multiplication operator, right-multiplies a BezierPatch by an ordinary vector (a parameter vector).
 * This can be used to right-multiply a Bezier geometry matrix by a parameter vector. 
 * \param bezierpatch - The BezierPatch that should be multimplied.
 * \param vector - The vector (a parameter vector) that is right-multiplied by the bezier patch.
 * \return The product bezierpatch * vector which is of type BezierColumn.
 */
BezierColumn operator*(BezierPatch const& bezierpatch, glm::vec4 const& vector);

/**
 * Multiplication operator, left-multilpies a BezierPatch by en ordinary vector (a parameter vector).
 * This can be used to left-multiply a BezierPatch by a parameter vector. 
 * \param bezierpatch - The BezierPatch that should be multimplied.
 * \param vector - The vector (a parameter vector) that is left-multiplied by the bezier patch.
 * \return The product bezierpatch * vector which is of type BezierRow.
 */
BezierRow operator*(glm::vec4 const& vector, BezierPatch const& bezierpatch);

/**
 * Multiplication operator, right-multiplies a BezierRow by an ordinary vector (a parameter vector).
 * This can be used to right-multiply a BezierRow by a parameter vector. 
 * \param bezierrow - The BezierRow that should be multimplied.
 * \param vector - The vector (a parameter vector) that is right-multiplied by the bezierrow.
 * \return The product bezierrow * vector which is of type glm::vec3.
 */
glm::vec3 operator*(BezierRow const& bezierrow, glm::vec4 vector);

/**
 * Multiplication operator, left-multiplies a BezierColumn by an ordinary vector (a parameter vector).
 * This can be used to left-multiply a BezierColumn by a parameter vector. 
 * \param beziercolumn - The BezierColumn that should be multimplied.
 * \param vector - The vector (a parameter vector) that is left-multiplied by the beziercolumn.
 * \return The product beziercolumn * vector which is of type glm::vec3.
 */
glm::vec3 operator*(glm::vec4 const& vector, BezierColumn const& beziercolumn);

/**
 * Insertion operator, inserts a BezierPatch into an ostream.
 * \param s - The ostream which the geometryvector should be inserted into.
 * \param bezierpatch - The BezierPatch that should be inserted into the ostream.
 * \return The ostream which the bezierpatch has been inserted into.
 */
std::ostream& operator<<(std::ostream& s, BezierPatch const& bezierpatch);

#endif
