#ifndef READBEZIERPATCHES_H
#define READBEZIERPATCHES_H

/*******************************************************************\
*                                                                   *
*                 R e a d B e z i e r P a t c h e s                 *
*                                                                   *
\*******************************************************************/

#include <iostream>
#include <iomanip>
#include <stdexcept>

#include <fstream>
#include <vector>

#include <cstdio>
#include <cstring>

#include "bezierpatch.h"

int ReadBezierPatches(char const* filename, std::vector<BezierPatch>& BezierPatches);

#endif
