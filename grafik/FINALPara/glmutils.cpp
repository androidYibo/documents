#include "glm/glm.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "glmutils.h"


/**
 * Make it look like these functions are part of the glm library
 */
namespace glm {
    /**
     * Creates a 4x4 matrix which performs an XY-shear
     * \param sh_x - the shear factor in the x-direction
     * \param sh_y - the shear factor in the y-direction
     * \return a 4x4 matrix performing an XY shear
     */
    glm::mat4x4 shearXY(float sh_x, float sh_y)
    {
	glm::vec4 shearparam(sh_x, sh_y, 1.0f, 0.0f);
	glm::mat4x4 SHxy(glm::column(glm::mat4x4(1.0f), 2, shearparam));
	
	return SHxy;
    }

    /**
     * Creates a 4x4 matrix which performs an XZ-shear
     * \param sh_x - the shear factor in the x-direction
     * \param sh_z - the shear factor in the z-direction
     * \return a 4x4 matrix performing an XZ shear
     */
    glm::mat4x4 shearXZ(float sh_x, float sh_z)
    {
	glm::vec4 shearparam(sh_x, 1.0f, sh_z, 0.0f);
	glm::mat4x4 SHxz(glm::column(glm::mat4x4(1.0f), 1, shearparam));
	
	return SHxz;
    }

    /**
     * Creates a 4x4 matrix which performs an YZ-shear
     * \param sh_y - the shear factor in the y-direction
     * \param sh_z - the shear factor in the z-direction
     * \return a 4x4 matrix performing an YZ shear
     */
    glm::mat4x4 shearYZ(float sh_y, float sh_z)
    {
	glm::vec4 shearparam(1.0f, sh_y, sh_z, 0.0f);
	glm::mat4x4 SHyz(glm::column(glm::mat4x4(1.0f), 0, shearparam));
	
	return SHyz;
    }
}


/**
 * Utility operators
 */

/**
 * Prints a vec2 to std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::vec2& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::vec2 const& v)
{
    s << ' ';
    for (int i = 0; i < 2; ++i) {
	s << std::setw(6) << std::setprecision(4) << v[i] << ' ';
    }
    return s;
}


/**
 * Prints a vec3 to std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::vec3& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::vec3 const& v)
{
    s << ' ';
    for (int i = 0; i < 3; ++i) {
	s << std::setw(6) << std::setprecision(4) << v[i] << ' ';
    }
    return s;
}


/**
 * Prints a vec4 to std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::vec4& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::vec4 const& v)
{
    for (int i = 0; i < 4; ++i) {
	s << std::setw(6) << std::setprecision(4) << v[i] << ' ';
    }
    return s;
}

/**
 * Prints a mat4x4t o std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::mat4x4& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::mat4x4 const& m)
{
    for (int r = 0; r < 4; ++r) {
	s << ' ';
	for (int c = 0; c < 4; ++c) {
	    s << std::setw(6) << std::setprecision(4) << m[c][r] << ' ';
	}
	s << std::endl;
    }
    return s;
}


