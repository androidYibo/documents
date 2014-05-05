#include "bezierpatch.h"

/**
 * \class BezierRow 
 * implements the concept of a row of a geometry matrix for a parametric surface where
 * each coordinate function is a polynomial of degree 3.
 */

/**
 * Default constructor creates an empty BezierRow, i.e. all entries are the zero vector.
 */
BezierRow::BezierRow()
{
    //std::cout << "-->BezierRow::BezierRow()" << std::endl;

    glm::vec3 zeroes(0.0f);
    for (int i = 0; i < 4; ++i) this->controlpoints[i] = zeroes;

    //std::cout << "<--BezierRow::BezierRow()" << std::endl;
}

/**
 * Parameterized constructor creates a BezierRow containing the arguments as entries.
 * \param G1 - Entry one in the geometry row vector.
 * \param G2 - Entry two in the geometry row vector.
 * \param G3 - Entry three in the geometry row vector.
 * \param G4 - Entry four in the geometry row vector.
 */
BezierRow::BezierRow(glm::vec3 const& G1, glm::vec3 const& G2, glm::vec3 const& G3, glm::vec3 const& G4)
{
    this->controlpoints[0] = G1;
    this->controlpoints[1] = G2;
    this->controlpoints[2] = G3;
    this->controlpoints[3] = G4;
}

/**
 * Copy constructor creates a new BezierRow which is a copy of its argument.
 * \param bezierrow - The geometry row vector to be copied.
 */
BezierRow::BezierRow(BezierRow const& bezierrow)
{
    for (int i = 0; i < 4; ++i) this->controlpoints[i] = bezierrow.controlpoints[i];
}

/**
 * Destructor destroys the current instance of BezierRow.
 */
BezierRow::~BezierRow()
{}

/**
 * Assignent operator assigns its argument to the current instance of BezierRow.
 * \lparam bezierrow - The geometry row vector to be assigned to this instance.
 */
BezierRow& BezierRow::operator=(BezierRow const& bezierrow)
{
    if (this != &bezierrow) {
	for (int i = 0; i < 4; ++i) this->controlpoints[i] = bezierrow.controlpoints[i];
    }
    return *this;
}

/**
 * Index operator - read only - returns the i'th entry in the geometry row vector,
 * \param i - The index of the entry to be returned.
 */
glm::vec3 const& BezierRow::operator[](int i) const
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierRow::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlpoints[i - 1];
}

/**
 * Index operator returns a reference to the i'th entry in the geometry row vector.
 * \param i - The index of the entry to which a reference is to be returned;
 */
glm::vec3& BezierRow::operator[](int i)
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierRow::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlpoints[i - 1];
}


/**
 * \class BezierColumn
 * implements the concept of a column of a geometry matrix for a parametric surface where
 * each coordinate function is a polynomial of degree 3.
 */

/**
 * Default constructor creates an empty BezierColumn, i.e. all entries are the zero vector.
 */
BezierColumn::BezierColumn()
{
    //std::cout << "-->BezierColumn::BezierColumn()" << std::endl;

    glm::vec3 zeroes(0.0f);
    for (int i = 0; i < 4; ++i) this->controlpoints[i] = zeroes;

    //std::cout << "<--BezierColumn::BezierColumn()" << std::endl;
}

/**
 * Parameterized constructor creates a BezierColumn containing the arguments as entries.
 * \param G1 - Entry one in the geometry column vector.
 * \param G2 - Entry two in the geometry column vector.
 * \param G3 - Entry three in the geometry column vector.
 * \param G4 - Entry four in the geometry column vector.
 */
BezierColumn::BezierColumn(glm::vec3 const& G1, glm::vec3 const& G2, glm::vec3 const& G3, glm::vec3 const& G4)
{
    this->controlpoints[0] = G1;
    this->controlpoints[1] = G2;
    this->controlpoints[2] = G3;
    this->controlpoints[3] = G4;   
}

/**
 * Copy constructor creates a new BezierColumn which is a copy of its argument.
 * \param beziercolumn - The geometry column vector to be copied.
 */
BezierColumn::BezierColumn(BezierColumn const& beziercolumn)
{
    for (int i = 0; i < 4; ++i) this->controlpoints[i] = beziercolumn.controlpoints[i];
}

/**
 * Destructor destroys the current instance of BezierColumn.
 */
BezierColumn::~BezierColumn()
{}

/**
 * Assignent operator assigns its argument to the current instance of BezierColumn.
 * \lparam beziercolum - The geometry column vector to be assigned to this instance.
 */
BezierColumn& BezierColumn::operator=(BezierColumn const& beziercolumn)
{
    if (this != &beziercolumn) {
	for (int i = 0; i < 4; ++i) this->controlpoints[i] = beziercolumn.controlpoints[i];
    }
    return *this;
}

/**
 * Index operator - read only - returns the i'th entry in the geometry column vector,
 * \param i - The index of the entry to be returned.
 */
glm::vec3 const& BezierColumn::operator[](int i) const
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierColumn::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlpoints[i - 1];
}

/**
 * Index operator returns a reference to the i'th entry in the geometry column vector.
 * \param i - The index of the entry to which a reference is to be returned;
 */
glm::vec3& BezierColumn::operator[](int i)
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierColumn::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlpoints[i - 1];
}



/**
 * \class BezierPacth 
 *implements the concept of a geometry matrix for a parametric surface where each 
 * coordinate function is a polynomial of degree 3.
 */

/**
 * Default constructor creates an empty BezierPatch, i.e. all entries are the zero vector.
 */
BezierPatch::BezierPatch()
{
    //std::cout << "-->BezierPatch::BezierPatch()" << std::endl;

    //std::cout << "<--BezierPatch::BezierPatch()" << std::endl;
}

/**
 * Parameterized constructor creates a BezierPatch containing the arguments as entries.
 * \param G11,..., G44 - The entries of the geometry matrix.
 */
BezierPatch::BezierPatch(glm::vec3 g11, glm::vec3 g12, glm::vec3 g13, glm::vec3 g14,
	                 glm::vec3 g21, glm::vec3 g22, glm::vec3 g23, glm::vec3 g24,
	                 glm::vec3 g31, glm::vec3 g32, glm::vec3 g33, glm::vec3 g34,
	                 glm::vec3 g41, glm::vec3 g42, glm::vec3 g43, glm::vec3 g44)
{
    this->controlvec[0] = BezierRow(g11, g12, g13, g14);
    this->controlvec[1] = BezierRow(g21, g22, g23, g24);
    this->controlvec[2] = BezierRow(g31, g32, g33, g34);
    this->controlvec[3] = BezierRow(g41, g42, g43, g44);
}

/**
 * Copy constructor creates a new BezierPatch which is a copy of its argument.
 * \param bezierpatch - The bezierpatch to be copied.
 */
BezierPatch::BezierPatch(BezierPatch const& bezierpatch)
{
    for (int i = 0; i < 4; ++i) {
	this->controlvec[i] = bezierpatch.controlvec[i];
    }
}

/**
 * Destructor destroys the current instance of BezierPatch.
 */
BezierPatch::~BezierPatch()
{}

/**
 * Assignent operator assigns its argument to the current instance of BezierPatch.
 * \lparam bezierpatch - The bezierpatch to be assigned to this instance.
 */
BezierPatch& BezierPatch::operator=(BezierPatch const& bezierpatch)
{
    if (this != &bezierpatch) {
	for (int i = 0; i < 4; ++i) {
	    this->controlvec[i] = bezierpatch.controlvec[i];
	}
    }
    return *this;
}

/**
 * Index operator - read only - returns the i'th row of the geometry matrix,
 * \param i - The index of the row to be returned.
 */
BezierRow const& BezierPatch::operator[](int i) const
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierPatch::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlvec[i - 1];
}

/**
 * Index operator returns a reference to the i'th row iof the geometry matrix.
 * \param i - The index of the row to which a reference is to be returned;
 */
BezierRow& BezierPatch::operator[](int i)
{
    if ((i < 1) && (i > 4)) {
	throw std::out_of_range("BezierPatch::operator[](int): The index must be in the range {1,...,4}");
    }
    return this->controlvec[i - 1];
}


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
BezierPatch operator*(BezierPatch const& bezierpatch, glm::mat4x4 const& matrix)
{
    BezierPatch result;

    for (int i = 1; i <= 4; ++i) {
	for (int j = 1; j <= 4; ++j) {
	    glm::vec4 column(glm::column(matrix, j - 1));
	    result[i][j] = bezierpatch[i] * column;
	}
    }
    return result;
}

/**
 * Multiplication operator, left-multiplies a BezierPatch by an ordinary matrix (a basis matrix).
 * This can be used to left-multiply a BezierPatch by a basis matrix. 
 * \param bezierpatch - The BezierPatch that should be multimplied.
 * \param matrix - The matrix (a basis matrix) that is left-multiplied by the bezier patch.
 * \return The product matrix * bezierpatch which is of type BezierPatch.
 */
BezierPatch operator*(glm::mat4x4 const& matrix, BezierPatch const& bezierpatch)
{
    BezierPatch result;

    for (int i = 1; i <= 4; ++i) {
	for (int j = 1; j <= 4; ++j) {
	    glm::vec4 row(glm::row(matrix, i - 1));

	    glm::vec3 tmpres(0.0f);
	    for (int k = 1; k <= 4; ++k) {
		tmpres += row[k - 1] * bezierpatch[k][j];
	    }
	    result[i][j] = tmpres;
	}
    }
    return result;   
}

/**
 * Multiplication operator, right-multiplies a BezierPatch by an ordinary vector (a parameter vector).
 * This can be used to right-multiply a Bezier geometry matrix by a parameter vector. 
 * \param bezierpatch - The BezierPatch that should be multimplied.
 * \param vector - The vector (a parameter vector) that is right-multiplied by the bezier patch.
 * \return The product bezierpatch * vector which is of type BezierColumn.
 */
BezierColumn operator*(BezierPatch const& bezierpatch, glm::vec4 const& vector)
{
    BezierColumn result;

    for (int i = 1; i <= 4; ++i) {
	result[i] = bezierpatch[i] * vector;
    }
    return result;
}

/**
 * Multiplication operator, left-multilpies a BezierPatch by en ordinary vector (a parameter vector).
 * This can be used to left-multiply a BezierPatch by a parameter vector. 
 * \param bezierpatch - The BezierPatch that should be multimplied.
 * \param vector - The vector (a parameter vector) that is left-multiplied by the bezier patch.
 * \return The product bezierpatch * vector which is of type BezierRow.
 */
BezierRow operator*(glm::vec4 const& vector, BezierPatch const& bezierpatch)
{
    BezierRow result;

    for (int j = 1; j <= 4; ++j) {
	glm::vec3 tmpres(0.0f);
	for (int i = 1; i <= 4; ++i) {
	    tmpres += vector[i - 1] * bezierpatch[i][j];
	}
	result[j] = tmpres;
    }
    return result;
}

/**
 * Multiplication operator, right-multiplies a BezierRow by an ordinary vector (a parameter vector).
 * This can be used to right-multiply a BezierRow by a parameter vector. 
 * \param bezierrow - The BezierRow that should be multimplied.
 * \param vector - The vector (a parameter vector) that is right-multiplied by the bezierrow.
 * \return The product bezierrow * vector which is of type glm::vec3.
 */
glm::vec3 operator*(BezierRow const& bezierrow, glm::vec4 vector)
{
    glm::vec3 result(0.0f);

    for (int i = 1; i <= 4; ++i) {
	result += bezierrow[i] * vector[i - 1];
    }
    return result;
}

/**
 * Multiplication operator, left-multiplies a BezierColumn by an ordinary vector (a parameter vector).
 * This can be used to left-multiply a BezierColumn by a parameter vector. 
 * \param beziercolumn - The BezierColumn that should be multimplied.
 * \param vector - The vector (a parameter vector) that is left-multiplied by the beziercolumn.
 * \return The product beziercolumn * vector which is of type glm::vec3.
 */
glm::vec3 operator*(glm::vec4 const& vector, BezierColumn const& beziercolumn)
{
    glm::vec3 result(0.0f);

    for (int i = 1; i <= 4; ++i) {
	result += vector[i - 1] * beziercolumn[i];
    }
    return result;
}

/**
 * Insertion operator, inserts a BezierPatch into an ostream.
 * \param s - The ostream which the geometryvector should be inserted into.
 * \param bezierpatch - The BezierPatch that should be inserted into the ostream.
 * \return The ostream which the bezierpatch has been inserted into.
 */
std::ostream& operator<<(std::ostream& s, BezierPatch const& bezierpatch)
{
    for (int i = 1; i <= 4; ++i) {
	s << ' ';
	for (int j = 1; j < 4; ++j) {
	    s << std::setw(6) << std::setprecision(4) << bezierpatch[i][j] << " | ";
	}
	s << std::setw(6) << std::setprecision(4) << bezierpatch[i][4] << ' ' << std::endl;
    }
    return s;
}
