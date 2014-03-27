#ifndef __GLM_UTILS_H__
#define __GLM_UTILS_H__

#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include "glm/glm.hpp"
#include "glm/gtc/matrix_access.hpp"

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
    glm::mat4x4 shearXY(float sh_x, float sh_y);

    /**
     * Creates a 4x4 matrix which performs an XZ-shear
     * \param sh_x - the shear factor in the x-direction
     * \param sh_z - the shear factor in the z-direction
     * \return a 4x4 matrix performing an XZ shear
     */
    glm::mat4x4 shearXZ(float sh_x, float sh_z);

    /**
     * Creates a 4x4 matrix which performs an YZ-shear
     * \param sh_y - the shear factor in the y-direction
     * \param sh_z - the shear factor in the z-direction
     * \return a 4x4 matrix performing an YZ shear
     */
    glm::mat4x4 shearYZ(float sh_y, float sh_z);
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
std::ostream& operator<<(std::ostream& s, glm::vec2 const& v);
    

/**
 * Prints a vec3 to std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::vec3& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::vec3 const& v);


/**
 * Prints a vec4 to std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::vec4& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::vec4 const& v);


/**
 * Prints a mat4x4t o std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::mat4x4& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::mat4x4 const& m);

#endif
