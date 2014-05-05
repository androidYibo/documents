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


void PrintIndexMatrix(int PatchNumber,
		      int index_11, int index_12, int index_13, int index_14,
		      int index_21, int index_22, int index_23, int index_24,
		      int index_31, int index_32, int index_33, int index_34,
		      int index_41, int index_42, int index_43, int index_44)
{
    // std::cout << "patch number: " 
		  // << std::setw(2) << PatchNumber << std::endl << std::flush;

    // std::cout << "patch indices: " << std::endl << '\t'
		  // << std::setw(6) << index_11 << ", " 
		  // << std::setw(6) << index_12 << ", " 
		  // << std::setw(6) << index_13 << ", " 
		  // << std::setw(6) << index_14 << std::endl;
		    
    // std::cout << '\t'
		  // << std::setw(6) << index_21 << ", " 
		  // << std::setw(6) << index_22 << ", "
		  // << std::setw(6) << index_23 << ", " 
		  // << std::setw(6) << index_24 << std::endl;
		    
    // std::cout << '\t'
		  // << std::setw(6) << index_31 << ", " 
		  // << std::setw(6) << index_32 << ", "
		  // << std::setw(6) << index_33 << ", " 
		  // << std::setw(6) << index_34 << std::endl;
		    
    // std::cout << '\t'
		  // << std::setw(6) << index_41 << ", " 
		  // << std::setw(6) << index_42 << ", "
		  // << std::setw(6) << index_43 << ", " 
		  // << std::setw(6) << index_44 << std::endl;
}


int ReadBezierPatches(char const* filename, std::vector<BezierPatch>& BezierPatches)
{
    // std::cout << "-->ReadBezierPatches(...)" << std::endl;

    // States
    int const NVERTEX        = 0;
    int const READ_VERTICES  = 1;
    int const PATCHNAME      = 2;
    int const SEARCH_PATCHES = 3;
    int const READ_PATCHES   = 4;

    char   ch;
    int    const MAX_BUFFER = 256;
    char   InputBuffer[MAX_BUFFER];
	
    int    NumberOfVertices;
    int    VertexNumber;
    float  x;
    float  y;
    float  z;

    char   PatchName[MAX_BUFFER];
    int    PatchNumber;
    int    index_11, index_12, index_13, index_14;
    int    index_21, index_22, index_23, index_24;
    int    index_31, index_32, index_33, index_34;
    int    index_41, index_42, index_43, index_44;

    // The vertex list
    std::vector<glm::vec3> Vertices;


    std::ifstream data_file(filename);
    if (!data_file) {
	std::cerr << "Cannot open data file: " << filename << std::endl << std::flush;
	throw std::runtime_error("Error on opening file");
    }
    
    int currentState = NVERTEX;
    while (data_file.get(InputBuffer, MAX_BUFFER, '\n')) {
	// std::cout << "InputBuffer: " << InputBuffer << std::endl;

	if (data_file.get(ch) && ch != '\n') {
	    data_file.close();
	    std::cerr << "Eof on data file: " << filename << std::endl << std::flush;
	    throw std::runtime_error("Eof on data file");
	}

	// Now one line of data is in InputBuffer
	// Use sscanf to extract the numeric values
	//
	// Build a data structure which represents the different
	// vertices and patches
	//
	// There are several possibilities for the input lines:
	//
	// 1: a comment line, i.e. InputBuffer[0] == '#'
	// 2: a 'number of vertices line', i.e. just one number
	//    sscanf(InputBuffer, "%d", &NumberOfVertices)
	// 3: a 'vertex line', i.e. 4 numbers
	//    sscanf(InputBuffer, "%d%f%f%f", &VertexNumber, &x, &y, &z)
	// 4: a 'patch line', i.e. 17 numbers
	//    sscanf(InputBuffer,
	//           "%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d",
	//           &PatchNumber,
	//           &index_11, &index_12, &index_13, &index_14,
	//           &index_21, &index_22, &index_23, &index_24,
	//           &index_31, &index_32, &index_33, &index_34,
	//           &index_41, &index_42, &index_43, &index_44)
	
	switch (currentState) {
	case NVERTEX: {
	    if (InputBuffer[0] != '#') {
		if (sscanf(InputBuffer, "%d", &NumberOfVertices) != 1) {
		    std::cerr << "number of vertices not found in data file: "
			      << filename << std::endl << std::flush;
		    throw std::runtime_error("Wrong number of vertices in file");
		}
		//std::cout << "Number of Vertices = " << NumberOfVertices << std::endl << std::endl;
		currentState = READ_VERTICES;
	    }
	    break;
	}
	case READ_VERTICES: {
	    if (InputBuffer[0] != '#') {
		if (sscanf(InputBuffer, "%d%f%f%f",
			   &VertexNumber, &x, &y, &z) != 4) {
		    std::cerr << "vertex not found in data file: " 
			      << filename << std::endl << std::flush;
		    throw std::runtime_error("vertex not found in data file");
		}
		else {
		    // insert the vertex in a data structure
	
		    glm::vec3 Vertex(x, y, z);
            // std::cout << "Vertex[" << VertexNumber << "] = [" << Vertex << "]" << std::endl;
		    Vertices.push_back(Vertex);

		    if (VertexNumber == NumberOfVertices) {
			//std::cout << "Number of Vertices in list: " << Vertices.size() << std::endl;
			currentState = PATCHNAME;
		    }
		}
	    }
	    break;
	}
	case PATCHNAME: {
	    if (InputBuffer[0] == '#') {
		if (strlen(InputBuffer) > 2) {
		    // read the name of the patch
		    
		    if (sscanf(InputBuffer, "# %s", PatchName) != 1) {
			std::cerr << "patch name not found" << std::endl << std::flush;
			throw std::runtime_error("patch name not found in data file");
		    }
			// std::cout << "patch name: " << PatchName << std::endl << std::flush;
		    currentState = SEARCH_PATCHES;
		}
	    }
	    break;
	}
	case SEARCH_PATCHES:
	case READ_PATCHES: {
	    if (InputBuffer[0] == '#') {
		if (currentState == READ_PATCHES) {
		    currentState = PATCHNAME;
		}
	    }
	    else {
		currentState = READ_PATCHES;
		if (sscanf(InputBuffer, "%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d",
			   &PatchNumber,
			   &index_11, &index_12, &index_13, &index_14,
			   &index_21, &index_22, &index_23, &index_24,
			   &index_31, &index_32, &index_33, &index_34,
			   &index_41, &index_42, &index_43, &index_44) != 17) {
		    std::cerr << "No patch found in data file: " << filename 
			      << std::endl << std::flush;
		    throw std::runtime_error("No patch found in data file");
		}
		else {
		    // insert patch in a data structure
#if 1
		    PrintIndexMatrix(PatchNumber,
				     index_11, index_12, index_13, index_14,
				     index_21, index_22, index_33, index_44,
				     index_31, index_32, index_33, index_34,
				     index_41, index_42, index_43, index_44);
#endif
		    
		    //std::cout << "Create a Patch" << std::endl;

		    BezierPatch BPatch;

		    // First row
		    BPatch[1][1] = Vertices[index_11-1];
		    BPatch[1][2] = Vertices[index_12-1];
		    BPatch[1][3] = Vertices[index_13-1];
		    BPatch[1][4] = Vertices[index_14-1];

		    // Second row
		    BPatch[2][1] = Vertices[index_21-1];
		    BPatch[2][2] = Vertices[index_22-1];
		    BPatch[2][3] = Vertices[index_23-1];
		    BPatch[2][4] = Vertices[index_24-1];
		    
		    // Third row
		    BPatch[3][1] = Vertices[index_31-1];
		    BPatch[3][2] = Vertices[index_32-1];
		    BPatch[3][3] = Vertices[index_33-1];
		    BPatch[3][4] = Vertices[index_34-1];
		    
		    // Forth row
		    BPatch[4][1] = Vertices[index_41-1];
		    BPatch[4][2] = Vertices[index_42-1];
		    BPatch[4][3] = Vertices[index_43-1];
		    BPatch[4][4] = Vertices[index_44-1];
		    
		    //std::cout << "Patch:" << std::endl;
		    //std::cout << BPatch << std::endl;

		    //std::cout << "Insert the patch" << std::endl;

		    BezierPatches.push_back(BPatch);
		    //std::cout << "Number of patches read: " << BezierPatches.size() << std::endl;
		}
	    }
	    break;
	}
	default: {
	    std::cout << "Default part of switch - something is wrong" << std::endl;
	    break;
	}
	}
    }
    if (!data_file.eof()) {
	std::cerr << "Something strange happend on data file: " << filename << std::endl;
	throw std::runtime_error("Something strange happend on data file");
    }
    data_file.close();
    
#if 0
    std::cout << "The vertices in the list of Vertices:" << std::endl;
    std::cout << "=====================================" << std::endl;
    for (int i = 0; i < Vertices.size(); ++i) {
	std::cout << "Vertex[" << std::setw(3) << i+1 << "]: [" << std::setw(6) << Vertices[i] << "]"
		  << std::endl;
    }
#endif

#if 0
    std::cout << "The Bezier Patches read:" << std::endl;
    std::cout << "========================" << std::endl;
    for (int i = 0; i < BezierPatches.size(); ++i) {
	std::cout << "Bezier Patch[" << std::setw(2) << i+1 << "]:" << std::endl;
	std::cout << "[" << std::endl;
	std::cout << BezierPatches[i] << "]" << std::endl << std::endl;
    }
#endif

    // std::cout << "<--ReadBezierPatches(...)" << std::endl;

    return 0;
}

