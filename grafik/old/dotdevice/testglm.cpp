#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include "glm/glm.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/type_ptr.hpp"
#include "glm/gtc/matrix_access.hpp"
#include "glm/gtx/transform.hpp"
#include "glm/gtx/transform2.hpp"
#include "glmutils.h"

#if 0
/**
 * Prints a vec2 to std::cout
 * \param s - a std::ostream& where the output should be written.
 * \param v - a glm::vec2& which should be written.
 * \return the parameter s.
 */
std::ostream& operator<<(std::ostream& s, glm::vec2 const& v) {
    // s << "operator<<(std::ostream&, glm::vec2&)" << std::endl;
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
std::ostream& operator<<(std::ostream& s, glm::vec3 const& v) {
    // s << "operator<<(std::ostream&, glm::vec3&)" << std::endl;
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
std::ostream& operator<<(std::ostream& s, glm::vec4 const& v) {
    // s << "operator<<(std::ostream&, glm::vec4&)" << std::endl;
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
std::ostream& operator<<(std::ostream& s, glm::mat4x4 const& m) {
    // s << "operator<<(std::ostream&, glm::vec4&)" << std::endl;
    for (int r = 0; r < 4; ++r) {
	s << ' ';
	for (int c = 0; c < 4; ++c) {
	    s << std::setw(6) << std::setprecision(4) << m[c][r] << ' ';
	}
	s << std::endl;
    }

    return s;
}
#endif

std::string horizline(60, '=');

glm::mat4x4 ComputeMperpar(glm::vec3 const& vrp, glm::vec3 const& vpn, glm::vec3 const& vup,
			   glm::vec3 const& prp,
			   glm::vec2 const& window_lower_left, glm::vec2 const& window_upper_right,
			   float front_clipping_plane, float back_clipping_plane)
{
    std::cout << "-->ComputeNper()" << std::endl;

    glm::vec3 n(glm::normalize(vpn));
    glm::vec3 u(glm::normalize(glm::cross(vup, vpn)));
    glm::vec3 v(glm::normalize(glm::cross(n, u)));

    std::cout << "Eye coordinate system:" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << "   vrp = " << vrp << std::endl;
    std::cout << "   vup = " << vup << std::endl;
    std::cout << std::endl;
    std::cout << "   u   = " << u << std::endl;
    std::cout << "   v   = " << v << std::endl;
    std::cout << "   n   = " << n << std::endl;
    std::cout << std::endl;
    std::cout << "   prp = " << prp << std::endl;
    std::cout << std::endl;
    std::cout << "   window lower left  = " << window_lower_left  << std::endl;
    std::cout << "   window upper right = " << window_upper_right << std::endl;
    std::cout << std::endl;
    std::cout << "   front clipping plane = " << std::setw(6) << std::setprecision(4)
	      << front_clipping_plane << std::endl;
    std::cout << "   back clipping plane  = " << std::setw(6) << std::setprecision(4)
	      << back_clipping_plane << std::endl;
    std::cout << std::endl;

    // Make the ViewOrientation Matrix
    std::cout << "Make the ViewOrientation Matrix" << std::endl;

    glm::mat4x4 T_vrp = glm::translate(-vrp);
    std::cout << "T(-vrp)" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << T_vrp << std::endl;

    glm::mat4x4 Rotation;
    glm::row(Rotation, 0, glm::vec4(u, 0.0f));
    glm::row(Rotation, 1, glm::vec4(v, 0.0f));
    glm::row(Rotation, 2, glm::vec4(n, 0.0f));

    std::cout << "Rotation(u, v, n)" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << Rotation << std::endl;

    // Here is the ViewOrientation Matrix
    glm::mat4x4 ViewOrientation = Rotation * T_vrp;
    std::cout << "ViewOrientation = Rotation * T_vrp" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << ViewOrientation << std::endl;



    // Make the ViewProjection Matrix
    std::cout << "Make the ViewProjection Matrix" << std::endl;

    // Translate top of the view pyramid to the origin
    glm::mat4x4 T_prp = glm::translate(-prp);
    std::cout << "T(-prp)" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << T_prp << std::endl;

    // Make the shear matrix SHxy
    glm::vec3 CW((window_upper_right + window_lower_left) / 2.0f, 0.0f);
    glm::vec3 DOP(prp - CW);
    float shx = 0.0f;
    float shy = 0.0f;
    if (DOP.z != 0.0f) {
	shx = -DOP.x / DOP.z;
	shy = -DOP.y / DOP.z;
    }
#if 0
    glm::vec4 shearparam(shx, shy, 1.0f, 0.0f);
    glm::mat4x4 SHxy(glm::column(glm::mat4x4(1.0f), 2, shearparam));
#else
    glm::mat4x4 SHxy(glm::shearXY(shx, shy));
#endif
    std::cout << "SHxy(DOP.u / DOP.n, DOP.v /DOP.n)" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << SHxy << std::endl;
    
    // Make the scale matrix S
    float sx = 2.0f * prp.z / (window_upper_right.x - window_lower_left.x);
    float sy = 2.0f * prp.z / (window_upper_right.y - window_lower_left.y);
    float s  = -1.0f / (back_clipping_plane - prp.z);

    glm::mat4x4 S(glm::scale(sx * s, sy * s, s));
    std::cout << "Scaling matrix" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << S << std::endl;

    glm::mat4x4 ViewProjection(S * SHxy * T_prp);
    std::cout << "ViewProjection = S * Shxy * T(-prp)" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << ViewProjection << std::endl;

    // Make the matrix Mperpar
    float Zmax = -(front_clipping_plane - prp.z) / (back_clipping_plane - prp.z);
    glm::mat4x4 Mperpar(glm::vec4(1.0f, 0.0f, 0.0f, 0.0f),
			glm::vec4(0.0f, 1.0f, 0.0f, 0.0f),
			glm::vec4(0.0f, 0.0f, 1.0f / (1 + Zmax), -1.0f),
			glm::vec4(0.0f, 0.0f, -Zmax / (1.0f + Zmax), 0.0f));
    std::cout << "Mperpar" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << Mperpar << std::endl;

    glm::mat4x4 Mtotal(Mperpar * ViewProjection * ViewOrientation);
    std::cout << "Mtotal = Mperpar * ViewProjection * ViewOrientation" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << Mtotal << std::endl;

#if 0
    std::cout << "Test of the extreme points" << std::endl;
    std::cout << horizline << std::endl;
    glm::vec4 p(glm::vec4(1.0f, 1.0f, -1.0f, 1.0f));
    glm::vec4 pt = Mperpar * p;
    std::cout << "p = [" << p << "] --> [" << pt << "] --> [" << p / p.w << "]" << std::endl;

    glm::vec4 q(glm::vec4(-Zmax, -Zmax, Zmax, 1.0f));
    glm::vec4 qt = Mperpar * q;
    std::cout << "q = [" << q << "] --> [" << qt << "] --> [" << qt / qt.w << "]" << std::endl;
#endif
    std::cout << std::endl;
    std::cout << "<--ComputeNper()" << std::endl;

    std::cout << std::endl;

    // return the final matrix Mperpar * Nper
    return Mtotal;
}


int main() 
{
#if 0
    glm::vec4 v(0.0f, 0.0f, 0.0f, 1.0f);
    std::cout << v << std::endl;

    glm::mat4x4 m = glm::translate(1.0f, 2.0f, 3.0f);
    glm::vec4 w = m * v;
    std::cout << m << std::endl;
    std::cout << w << std::endl;

    glm::vec3 t(3.0f, 2.0f, 1.0f);
    glm::mat4x4 T = glm::translate(t);
    std::cout << T << std::endl;
#endif

    glm::vec3 VRP(0.0f, 0.0f, 0.0f);
    glm::vec3 VPN(0.0f, 0.0f, 1.0f);
    glm::vec3 VUP(0.0f, 1.0f, 0.0f);
    glm::vec3 PRP(0.0f, 0.0f, 5.0f);

    glm::vec2 window_lower_left(-4.0f, -3.0f);
    glm::vec2 window_upper_right(4.0f,  3.0f);

    float front_clipping_plane =  -5.0f;
    float back_clipping_plane  = -15.0f;


    glm::mat4x4 Mtotal = ComputeMperpar(VRP, VPN, VUP,
					PRP,
					window_lower_left, window_upper_right,
					front_clipping_plane, back_clipping_plane);

    std::cout << "Mtotal" << std::endl;
    std::cout << horizline << std::endl;
    std::cout << Mtotal << std::endl;

    std::cout << "Test of the extreme points" << std::endl;
    std::cout << horizline << std::endl;

    glm::vec4 Pxback(16.0f, 0.0f, -15.0f, 1.0f);
    glm::vec4 TPxback = Mtotal * Pxback;
    std::cout << "Pxback        = [" << Pxback  << "] --> [" << TPxback << "] --> ["
	      << TPxback / TPxback.w << "]" << std::endl;

    glm::vec4 Pyback(0.0f, 12.0f, -15.0f, 1.0f);
    glm::vec4 TPyback = Mtotal * Pyback;
    std::cout << "Pyback        = [" << Pyback   << "] --> [" << TPyback << "] --> ["
	      << TPyback / TPyback.w << "]" << std::endl;

    glm::vec4 Pbackcorner(16.0f, 12.0f, -15.0f, 1.0f);
    glm::vec4 TPbackcorner = Mtotal * Pbackcorner;
    std::cout << "Pbackcorner   = [" << Pbackcorner  << "] --> [" << TPbackcorner << "] --> ["
	      << TPbackcorner / TPbackcorner.w << "]" << std::endl;

    std::cout << std::endl;

    glm::vec4 Pxfront(8.0f, 0.0f, -5.0f, 1.0f);
    glm::vec4 TPxfront = Mtotal * Pxfront;
    std::cout << "Pxfront       = [" << Pxfront  << "] --> [" << TPxfront << "] --> ["
	      << TPxfront / TPxfront.w << "]" << std::endl;

    glm::vec4 Pyfront(0.0f, 6.0f, -5.0f, 1.0f);
    glm::vec4 TPyfront = Mtotal * Pyfront;
    std::cout << "Pyfront       = [" << Pyfront  << "] --> [" << TPyfront << "] --> [" 
	      << TPyfront / TPyfront.w << "]" << std::endl;

    glm::vec4 Pfrontcorner(8.0f, 6.0f, -5.0f, 1.0f);
    glm::vec4 TPfrontcorner = Mtotal * Pfrontcorner;
    std::cout << "Pfrontcorner  = [" << Pfrontcorner  << "] --> [" << TPfrontcorner << "] --> ["
	      << TPfrontcorner / TPfrontcorner.w << "]" << std::endl;



#if 0
    glm::mat4x4 Shx3d(glm::shearX3D(glm::mat4x4(1.0f), 10.0f, 20.0f));
    std::cout << "Shx3d = " << std::endl;
    std::cout << Shx3d << std::endl;

    glm::mat4x4 Shy3d(glm::shearY3D(glm::mat4x4(1.0f), 10.0f, 20.0f));
    std::cout << "Shy3d = " << std::endl;
    std::cout << Shy3d << std::endl;

    glm::mat4x4 Shz3d(glm::shearZ3D(glm::mat4x4(1.0f), 10.0f, 20.0f));
    std::cout << "Shz3d = " << std::endl;
    std::cout << Shz3d << std::endl;
#endif

    return 0;
}
