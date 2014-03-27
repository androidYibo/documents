#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <sstream>

#include <GL/glew.h>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>

#include <FL/Fl.H>
#include <FL/names.h>
#include <FL/fl_ask.H>

#include "fltkdevice.h"


const std::string sVertexShader = "                                                 \n\
    #version 110                                                                    \n\
                                                                                    \n\
    attribute vec3 aPosition;                                                       \n\
                                                                                    \n\
    uniform float uScale;                                                           \n\
    uniform mat4 uModelMatrix;                                                      \n\
                                                                                    \n\
    void main() {                                                                   \n\
        gl_Position = uModelMatrix * vec4(uScale * aPosition.xy, aPosition.z, 1.0); \n\
    }                                                                               \n\
";

const std::string sFragmentShader = "                                               \n\
    #version 110                                                                    \n\
                                                                                    \n\
    uniform vec3 uColour;                                                           \n\
                                                                                    \n\
    void main() {                                                                   \n\
        gl_FragColor = vec4(uColour, 1.0);                                          \n\
    }                                                                               \n\
";


/**
 * FltkDevice implements an OpenGL window using the toolkit Fltk
 *
 * Parameterized constructor, creates a window which can be used for drawing OpenGL graphics
 * \param X - the x-coordinate of the window position
 * \param Y - the y-coordinates of the window position
 * \param Width - the width of the window
 * \param Height - the height of the window
 * \param Title - the window title
 */
FltkDevice::FltkDevice(int const X, int const Y, int const Width, int const Height,
		       std::string const& Title)
          : Fl_Gl_Window(X, Y, Width, Height, Title.c_str()), 
	    glewInitialized(false),
	    Xorg(0), Yorg(0),
	    OrgWidth(Width), OrgHeight(Height),
	    pendingClear(true), pendingUpdate(true),
	    CurrentButton(Button_0)
{
    //std::cout << std::endl;
    //std::cout << "-->FltkDevice::FltkDevice(...)" << std::endl << std::flush;

    if ((Width == 0) || (Height == 0)) {
	throw std::runtime_error("FltkDevice::FltkDevice(...): The window has no area, i.e. one of its dimensions is zero");
    }

    this->AspectRatio = static_cast<float>(Width) / static_cast<float>(Height);

    this->mode(FL_RGB | FL_ALPHA | FL_DEPTH | FL_DOUBLE);

    //std::cout << "<--FltkDevice::FltkDevice(...)" << std::endl << std::flush;
}

/**
 * Destructor, cleans up the internal data structures and closes the window
 */
FltkDevice::~FltkDevice()
{
    this->CleanUp();
}

/**
 * Clears the window
 */
void FltkDevice::Clear()
{
    //std::cout << "      -->FltkDevice::Clear()" << std::endl << std::flush;

    this->pendingClear = true;
    this->Fl_Gl_Window::redraw();

    //std::cout << "      <--FltkDevice::Clear()" << std::endl << std::flush;
}

/**
 * Reset the internal data structures to default values
 */
void FltkDevice::Reset()
{}

/**
 * Clears the screen and redraws the contents of the window
 */
void FltkDevice::Update()
{
    //std::cout << "-->FltkDevice::Update()" << std::endl << std::flush;

    this->pendingClear  = false;
    this->pendingUpdate = true;
    this->Fl_Gl_Window::redraw();

    //std::cout << "<--FltkDevice::Update()" << std::endl << std::flush;
}

/**
 * A callback function
 */
void FltkDevice::Callback_1()
{
    //std::cout << "      -->FltkDevice::Callback_1()" << std::endl;
    
    fl_message("%s", "FltkDevice::Callback_1(): Called");
    this->CurrentButton = Button_1;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_1()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_2()
{
    //std::cout << "      -->FltkDevice::Callback_2()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_2(): Called");
    this->CurrentButton = Button_2;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_2()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_3()
{
    //std::cout << "      -->FltkDevice::Callback_3()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_3(): Called");
    this->CurrentButton = Button_3;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_3()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_4()
{
    //std::cout << "      -->FltkDevice::Callback_4()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_4(): Called");
    this->CurrentButton = Button_4;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_4()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_5()
{
    //std::cout << "      -->FltkDevice::Callback_5()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_5(): Called");
    this->CurrentButton = Button_5;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_5()" << std::endl;
}

 /**
 * A callback function
 */
void FltkDevice::Callback_6()
{
    //std::cout << "      -->FltkDevice::Callback_6()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_6(): Called");
    this->CurrentButton = Button_6;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_6()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_7()
{
    //std::cout << "      -->FltkDevice::Callback_7()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_7(): Called");
    this->CurrentButton = Button_7;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_7()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_8()
{
    //std::cout << "      -->FltkDevice::Callback_8()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_8(): Called");
    this->CurrentButton = Button_8;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_8()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_9()
{
    //std::cout << "      -->FltkDevice::Callback_9()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_9(): Called");
    this->CurrentButton = Button_9;
    this->Update();

    //std::cout << "      <--FltkDevice::Callback_9()" << std::endl;
}


/**
 * Protected functions
 */


/**
 * Private functions
 */

/**
 * Renders the shaped known to the device at the moment.
 * This is where all the drawing of geometric shapes id done.
 */
void FltkDevice::RenderShapes()
{
    //std::cout << "-->FltkDevice::RenderShapes()" << std::endl;
    /**
     * The actual update are here
     */
    //glColor4f(0.0, 0.0, 1.0, 1.0);

    switch (this->CurrentButton) {
    case Button_0:
	std::cout << "   Button 0" << std::endl;
	break;
    case Button_1:
        {
	    //std::cout << "   Button 1" << std::endl;

	    triangleShaderProgram.useProgram();

	    glm::mat4 modelMatrix;
	    glUniformMatrix4fv(glGetUniformLocation(triangleShaderProgram.getProgram(), "uModelMatrix"),
			       1, GL_FALSE, glm::value_ptr(modelMatrix));

	    glUniform1f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uScale"), 0.75f);
	    glUniform3f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uColour"), 0.0f, 1.0f, 0.0f);
	    triangle.draw();

	    glUniform1f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uScale"), 0.5f);
	    glUniform3f(glGetUniformLocation(triangleShaderProgram.getProgram(), "uColour"), 1.0f, 0.0f, 0.0f);
	    triangle.draw();
	}
	break;
    case Button_2:
	//std::cout << "   Button 2" << std::endl;
	
	this->Clear();
	break;
    case Button_3:
	//std::cout << "   Button 3" << std::endl;

	this->Clear();
	break;
    case Button_4:
	//std::cout << "   Button 4" << std::endl;

	this->Clear();
	break;
    case Button_5:
	//std::cout << "   Button 5" << std::endl;

	this->Clear();
	break;
    case Button_6:
	//std::cout << "   Button 6" << std::endl;

	this->Clear();
	break;
    case Button_7:
	//std::cout << "   Button 7" << std::endl;

	this->Clear();
	break;
    case Button_8:
	//std::cout << "   Button 8" << std::endl;

	this->Clear();
	break;
    case Button_9:
	//std::cout << "   Button 9" << std::endl;

	this->Clear();
	break;
    default:
	std::cout << "   No such button" << std::endl;
    }
      
    this->Update();

    //std::cout << "<--FltkDevice::RenderShapes()" << std::endl;
}

/**
 * Initializes the OpgnGL context
 */
void FltkDevice::InitializeGL()
{
    //std::cout << "-->FltkDevice::InitializeGL()" << std::endl << std::flush;

    glClearColor(0.0, 0.0, 1.0, 0.5);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL); // You might change this when the transformations are in place!

    glDrawBuffer(GL_BACK);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    this->swap_buffers();
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    this->ErrorCheck();

    //std::cout << "<--FltkDevice::InitializeGL()" << std::endl << std::flush;
}

/**
 * FORGET THIS FOR NOW!
 *
 * Initializes the viewport so it is centered in the window
 * \param Width - The new width of the window.
 * \param Height - The new height of the window.
 */
void FltkDevice::InitializeViewport(int Width, int Height)
{
    //std::cout << "-->FltkDevice::InitializeViewport(int, int)" << std::endl;

    // Compute a new viewport with the same aspect ratio as the original    
    // The original aspect ratio was: w_0 / h_0
    // And the new aspect ratio is:   w / h

    // Setup the new viewport

    //std::cout << "<--FltkDevice::InitializeViewport(int, int)" << std::endl;
}

/**
 * Initializes the GLEW library
 */
void FltkDevice::InitializeGLEW()
{
    //std::cout << "-->InitializeGLEW()" << std::endl;

    if (!this->glewInitialized) {
	GLenum err = glewInit();
	if (err != GLEW_OK) {
	    std::ostringstream errmess;
	    errmess << "   FltkDevice::Initialize(): "
		    << "GLEW failed to initialize: "
		    << glewGetErrorString(err) << ", (" << err << ") \n"
		    << "Status, Using GLEW version: " << glewGetString(GLEW_VERSION)
		    << std::endl;
	    std::cout << errmess.str();
	    this->glewInitialized = false;
	}
	else {
	    this->glewInitialized = true;

            //std::cout << "   GLEW version: " << glewGetString(GLEW_VERSION)
            //        << std::endl << std::flush;
#if 0
	    bool hardwareShaderSupport = GLEW_VERSON_2_0;
	    hardwareShaderSupport = GLEW_VERSION_2_0;
	    if (!hardwareShaderSupport) {
		std::cout << "Your graphics hardware/driver setup does not support OpenGL 2.0+." << std::endl;
		std::cout << "Your setup supports OpenGL: " << glGetString(GL_VERSION) << std::endl;
		std::cout << "Press enter to exit" << std::endl;
		std::cin.ignore();
		exit(-1);
	    }
#endif
	}
    }
    //std::cout << "<--InitializeGLEW()" << std::endl;
}

/**
 * Initializes the Shader Programs
 */
void FltkDevice::InitializeShadersAndGLObjects() {
    this->triangleShaderProgram.init(sVertexShader, sFragmentShader);
    this->triangle.initializeBuffers(this->triangleShaderProgram);
}

/**
 * Checks if any errors occurred during processing of OpenGL requests
 * If an error has occurred an exception is thown
 */
void FltkDevice::ErrorCheck() const
{
    // std::cout << "-->FltkDevice::ErrorCheck()" << std::endl << std::flush;

    bool AnyErrors = false;
    GLenum ErrorCode = GL_NO_ERROR;
    std::ostringstream errormessage;
    errormessage << "FltkDevice::ErrorCheck: OpenGL Rendering Failure: " << std::endl;
    while ((ErrorCode = glGetError()) != GL_NO_ERROR) {
	AnyErrors = true;
	errormessage << "FltkDevice::ErrorCheck: " << gluErrorString(ErrorCode) << std::endl;
    }
    if (AnyErrors) {
	throw std::runtime_error(errormessage.str());
    }

    // std::cout << "<--FltkDevice::ErrorCheck()" << std::endl << std::flush;
}

/**
 * Physically clears the window
 */
void FltkDevice::clear()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}



/**
 * Cleans up the internal data structures
 */
int FltkDevice::CleanUp()
{
    return 0;
}

/**
 * Virtual functions implemented to suit this class
 */

/**
 * Adjusts the internal parameters after the window has been resized
 * \param X - the new x-position of the window
 * \param Y - the new y-position of the window
 * \param Width - the new width of the window
 * \param Height - the new height of the window
 */
void FltkDevice::resize(int X, int Y, int Width, int Height)
{
    //std::cout << "-->FltkDevice::resize(...)" << std::endl << std::flush;

    if ((Width== 0) || (Height == 0)) {
	throw std::runtime_error("FltkDevice::resize(...): The window has no area, i.e. one of its dimensions is zero");
    }

    this->Fl_Gl_Window::resize(X, Y, Width, Height);
    this->InitializeViewport(Width, Height);
    this->Update();

    //std::cout << "<--FltkDevice::resize(...)" << std::endl << std::flush;
}

/**
 * Does the actual drawing of the contents of the window using OpenGL commands
 */
void FltkDevice::draw()
{
    //std::cout << "-->FltkDevice::draw()" << std::endl << std::flush;

    if (!this->Fl_Gl_Window::valid()) {
	this->InitializeGL();
	this->InitializeGLEW();
	this->InitializeShadersAndGLObjects();
    }

    this->clear();
    if (this->pendingClear) {
	this->swap_buffers();
	this->clear();
	this->pendingClear = false;
    }
    else {
	this->RenderShapes();
    }

    this->ErrorCheck();

    //std::cout << "<--FltkDevice::draw()" << std::endl << std::flush;
}

/**
 * Handles all events which occur while the window is in focus
 * \param Event - the Id of the event
 */
int FltkDevice::handle(int Event)
{

#if 0
    "FL_NO_EVENT",
 +  "FL_PUSH",
 +  "FL_RELEASE",
 +  "FL_ENTER",
 +  "FL_LEAVE",
 +  "FL_DRAG",
 +  "FL_FOCUS",
 +  "FL_UNFOCUS",
 +  "FL_KEYDOWN",
 +  "FL_KEYUP",
 +  "FL_CLOSE",
 +  "FL_MOVE",
 +  "FL_SHORTCUT",
 +  "FL_DEACTIVATE",
 +  "FL_ACTIVATE",
 +  "FL_HIDE",
 +  "FL_SHOW",
 +  "FL_PASTE",
 +  "FL_SELECTIONCLEAR",
    "FL_MOUSEWHEEL",
    "FL_DND_ENTER",
    "FL_DND_DRAG",
    "FL_DND_LEAVE",
    "FL_DND_RELEASE",
 +  "FL_SCREEN_CONFIGURATION_CHANGED",
 +  "FL_FULLSCREEN"
#endif

#if 0
    std::cout << "Event was " << fl_eventnames[Event] << ", (" << Event << ")"
	      << std::endl << std::flush;
#endif

    int result = 0;

    switch (Event) {
    case FL_SHORTCUT:
        {
	    result = this->HandleShortCutEvent(Fl::event_key());
	    if (result == 0) {
		result = this->Fl_Gl_Window::handle(Event);
	    }
	}
        break;
    default:
        result = this->Fl_Gl_Window::handle(Event);
    }

    return result;
}

/**
 * Handles a key press event
 */
int FltkDevice::HandleShortCutEvent(int const Key)
{
    //std::cout << "-->FltkDevice::HandleShortCutEvent(int const Key)" << std::endl << std::flush;

    //std::cout << "Input character was: " << static_cast<char>(Key) << std::endl << std::flush;

    int result = 0;

    switch (Key) {
      case '0':
	  this->CurrentButton = Button_0;
	  result = 1;
	  break;
      case '1':
	  this->CurrentButton = Button_1;
	  result = 1;
	  break;
      case '2':
	  this->CurrentButton = Button_2;
	  result = 1;
	  break;
      case '3':
	  this->CurrentButton = Button_3;
	  result = 1;
	  break;
      case '4':
	  this->CurrentButton = Button_4;
	  result = 1;
	  break;
      case '5':
	  this->CurrentButton = Button_5;
	  result = 1;
	  break;
      case '6':
	  this->CurrentButton = Button_6;
	  result = 1;
	  break;
      case '7':
	  this->CurrentButton = Button_7;
	  result = 1;
	  break;
      case '8':
	  this->CurrentButton = Button_8;
	  result = 1;
	  break;
      case '9':
	  this->CurrentButton = Button_9;
	  result = 1;
	  break;
      case 'q':
      case 'Q':
	  result = this->Quit();
	  break;
      default:
	  result = 0;
	  break;
    }

    this->Update();

    //std::cout << "<--FltkDevice::HandleShortCutEvent(int const Key)" << std::endl << std::flush;

    return result;
}

/**
 * Cleans up and exits
 */
int FltkDevice::Quit()
{
    this->CleanUp();
    exit(0);
}

