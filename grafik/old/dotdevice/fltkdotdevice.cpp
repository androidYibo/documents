#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <sstream>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#include <FL/Fl.H>
#include <FL/names.h>
#include <FL/fl_ask.H>

#include "fltkdotdevice.h"


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
	    OrgWidth(Width), OrgHeight(Height), AspectRatio(1),
	    pendingClear(true), pendingUpdate(true),
	    CurrentButton(Button_0),
	    unitLength(50), gridOn(true)
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
    // std::cout << "      -->FltkDevice::Callback_1()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_1(): Called");
    this->CurrentButton = Button_1;
    this->Update();

    // std::cout << "      <--FltkDevice::Callback_1()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_2()
{
    // std::cout << "      -->FltkDevice::Callback_2()" << std::endl;
    // fl_message("%s", "FltkDevice::Callback_2(): Called");

    this->CurrentButton = Button_2;

    this->Update();

    // std::cout << "      <--FltkDevice::Callback_2()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_3()
{
    //std::cout << "      -->FltkDevice::Callback_3()" << std::endl;

    this->CurrentButton = Button_3;
    this->Update();
    // fl_message("%s", "FltkDevice::Callback_3(): Called");

    //std::cout << "      <--FltkDevice::Callback_3()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_4()
{
    //std::cout << "      -->FltkDevice::Callback_4()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_4(): Called");

    //std::cout << "      <--FltkDevice::Callback_4()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_5()
{
    //std::cout << "      -->FltkDevice::Callback_5()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_5(): Called");

    //std::cout << "      <--FltkDevice::Callback_5()" << std::endl;
}

 /**
 * A callback function
 */
void FltkDevice::Callback_6()
{
    //std::cout << "      -->FltkDevice::Callback_6()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_6(): Called");

    //std::cout << "      <--FltkDevice::Callback_6()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_7()
{
    //std::cout << "      -->FltkDevice::Callback_7()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_7(): Called");

    //std::cout << "      <--FltkDevice::Callback_7()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_8()
{
    //std::cout << "      -->FltkDevice::Callback_8()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_8(): Called");

    //std::cout << "      <--FltkDevice::Callback_8()" << std::endl;
}

/**
 * A callback function
 */
void FltkDevice::Callback_9()
{
    //std::cout << "      -->FltkDevice::Callback_9()" << std::endl;

    fl_message("%s", "FltkDevice::Callback_9(): Called");

    //std::cout << "      <--FltkDevice::Callback_9()" << std::endl;
}


/**
 * Protected functions
 */


/**
 * Private functions
 */

/**
 * Draws a line
 */
void FltkDevice::DrawLine(int xstart, int ystart, 
                          int xend, int yend) {
    bool xneg = xstart > xend;

    if (xneg) { int t = xstart; xstart = xend; xend = t;
                    t = ystart; ystart = yend; yend = t;}

    bool yneg = ystart > yend;
    int dx  = xend - xstart;
    int dy  = yend - ystart;

    bool ydom = std::abs(dy) > std::abs(dx);
     
    if (ydom) { //checks if y is dominat and flip values if it is
        if (yneg) {
            int t = yend;
            yend = ystart;
            ystart = t;
            t = xend;
            xend = xstart;
            xstart = t;
            }
        this->DrawLineHelper(dy, dx, ystart, xstart, yend, ydom, yneg);}
    else {
        this->DrawLineHelper(dx, dy, xstart, ystart, xend, ydom, yneg);}
}


void FltkDevice::DrawLineHelper(int dx,  int dy, int x, int y, 
                                int end, bool ydom, bool yneg) {
    int dNE,dE;
    bool checkd;
    int d   = 2 * dy - dx;

    if (not yneg) {
        dNE = 2 * (dy - dx); //dNE
        dE  = 2 * dy;
        }
    else {
        if (ydom){
            dNE = 2 * (dy - dx); //dSE
            dE  = 2 * dx;
        }
        else {
            dNE = 2 * (dx - dy); //dSE
            dE  = 2 * dy;
        }
    }

    while (x <= end) {
        if (ydom){SetPixel(y, x, 0);} // Draws ydominant pixel
            else {SetPixel(x, y, 0);}

        if (yneg){checkd = (d <= 0);} // Difference between NE and SE calculations
            else {checkd = (d >  0);}

        if (checkd) {// Pick the NorthEast Cornor
            if (yneg){ y--;}
               else  { y++;}
        d =+ dNE;
        }
        else {
            d = d + dE;
            }
        x++;
        }
    this->Update();
    this->UnitLength(this->UnitLength(50));
}
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
    glColor4f(1.0, 1.0, 1.0, 1.0);

    switch (this->CurrentButton) {
    case Button_0:
	std::cout << "   Button 0" << std::endl;
	break;
    case Button_1:
        {
	    //std::cout << "   Button 1" << std::endl;

	    int OldUnitLength = this->UnitLength(50);
	    if (this->gridOn) {
            this->DrawGrid(1, 1);
            }
	    this->SetPixel(1, 2, 0);
	    this->Update();
	    this->UnitLength(OldUnitLength);
	}
	break;
    case Button_2:
	//std::cout << "   Button 2" << std::endl;
        {
        if (this->gridOn) {
            this->DrawGrid(1,1);
            }
        this-> TestLine(10,5,13,6, 1.0,0.0,0.0);
        this-> DrawLine(10,5,13,6);
        this-> TestLine(9,6,10,9, 1.0,0.0,0.0);
        this-> DrawLine(9,6,10,9); //red

        this-> TestLine(10,4,13,3, 0.0,1.0,0.0);
        this-> DrawLine(10,4,13,3);
        this-> TestLine(9,3,10,0, 0.0,1.0,0.0);
        this-> DrawLine(9,3,10,0); //green

        this-> TestLine(7,4,4,3, 1.0,1.0,1.0);
        this-> DrawLine(7,4,4,3);
        this-> TestLine(8,3,7,0, 1.0,1.0,1.0);
        this-> DrawLine(8,3,7,0); //white

        this-> TestLine(7,5,4,6, 0.0,0.0,1.0);
        this-> DrawLine(7,5,4,6);
        this-> TestLine(8,6,7,9, 0.0,0.0,1.0);
        this-> DrawLine(8,6,7,9); //blue
        }
	break;
    case Button_3:
        std::cout << "   Button 3" << std::endl;

        BasisMatrix(glm::vec4(-1.0f, 3.0f, -3.0f, 1.0f),
                    glm::vec4( 3.0f,-6.0f,  3.0f, 0.0f),
                    glm::vec4(-3.0f, 3.0f,  0.0f, 0.0f),
                    glm::vec4( 1.0f, 0.0f,  0.0f, 0.0f));
        // BezierVec4 GeometryVector(...)
        float t = 20;
        glm::vec4 ParameterVector(t*t*t,t*t,t,1.0f);

        glm::vec3 Point = GeometryVector * BasisMatrix * ParameterVector;

        glm::mat4x4 DLB(glm::vec4(8.0f, 0.0f, 0.0f, 0.0f),
                        glm::vec4(4.0f, 4.0f, 0.0f, 0.0f),
                        glm::vec4(2.0f, 4.0f, 2.0f, 0.0f),
                        glm::vec4(1.0f, 3.0f, 3.0f, 1.0f));

        DLB /= 8.0f;

        glm::mat4x4 DRB(glm::vec4(1.0f, 3.0f, 3.0f, 1.0f),
                        glm::vec4(0.0f, 2.0f, 4.0f, 2.0f),
                        glm::vec4(0.0f, 0.0f, 4.0f, 4.0f),
                        glm::vec4(0.0f, 0.0f, 0.0f, 8.0f));
        
        DRB /= 8.0f;

        BezierVec4 LeftGeometryVector  = GeometryVector * DLB;
        BezierVec4 RightGeometryVector = GeometryVector * DRB;
	break;
    case Button_4:
	//std::cout << "   Button 4" << std::endl;
	break;
    case Button_5:
	//std::cout << "   Button 5" << std::endl;
	break;
    case Button_6:
	//std::cout << "   Button 6" << std::endl;
	break;
    case Button_7:
	//std::cout << "   Button 7" << std::endl;
	break;
    case Button_8:
	//std::cout << "   Button 8" << std::endl;
	break;
    case Button_9:
	//std::cout << "   Button 9" << std::endl;
	break;
    default:
	std::cout << "   No such button" << std::endl;
    }
      
    this->pendingUpdate = false;
    //std::cout << "<--FltkDevice::RenderShapes()" << std::endl;
}

/**
 * Initializes the OpgnGL context
 */
void FltkDevice::InitializeGL()
{
    //std::cout << "-->FltkDevice::InitializeGL()" << std::endl << std::flush;

    glClearColor(0.5, 0.5, 0.5, 0.5);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, this->w(), 0, this->h(), -300, 300);
    this->InitializeViewport(this->w(), this->h());

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glEnable(GL_DEPTH_TEST);
    glClearDepth(4000.0);
    glDepthFunc(GL_LESS);

    glDrawBuffer(GL_BACK);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    this->swap_buffers();
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    this->ErrorCheck();

    //std::cout << "<--FltkDevice::InitializeGL()" << std::endl << std::flush;
}

/**
 * Initializes the viewport so it is centered in the window
 * \param Width - The new width of the window.
 * \param Height - The new height of the window.
 *
 * This does not work for UnitLength > 1.
 */
void FltkDevice::InitializeViewport(int Width, int Height)
{
    //std::cout << "-->FltkDevice::InitializeViewport(int, int)" << std::endl;

    // Compute a new viewport with the same aspect ratio as the original    
    // The original aspect ratio was: w_0 / h_0
    // And the new aspect ratio is:   w / h
    
    float NewWidth  = static_cast<float>(Width);
    float NewHeight = static_cast<float>(Height);
    if (Width > Height) {
	// Height determines the viewport height and Width is scaled
	NewWidth = Height * this->AspectRatio;
    }
    else {
	// Width determines the viewport width and Height is scaled
	NewHeight = Width / this->AspectRatio;
    }

    // Compute where tha viewport should be located
    float Xnew  = (Width  - NewWidth)  / 2.0;
    float Ynew  = (Height - NewHeight) / 2.0;

    // Setup the new viewport
    glViewport(static_cast<int>(Xnew), static_cast<int>(Ynew),
	       static_cast<GLsizei>(NewWidth), static_cast<GLsizei>(NewHeight));

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
	    //std::cout << "   GLEW version: " << glewGetString(GLEW_VERSION)
	    //	      << std::endl << std::flush;
	    this->glewInitialized = true;
	}
    }
    //std::cout << "<--InitializeGLEW()" << std::endl;
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

    std::cout << "Input character was: " << static_cast<char>(Key) << std::endl << std::flush;
    int result = 0;
    switch (Key) {
      case '0':
	  result = 1;
	  break;
      case '1':
	  result = 1;
	  break;
      case '2':
	  result = 1;
	  break;
      case '3':
	  result = 1;
	  break;
      case '4':
	  result = 1;
	  break;
      case '5':
	  result = 1;
	  break;
      case '6':
	  result = 1;
	  break;
      case '7':
	  result = 1;
	  break;
      case '8':
	  result = 1;
	  break;
      case '9':
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

    // std::cout << "<--FltkDevice::HandleShortCutEvent(int const Key)" << std::endl << std::flush;

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

/**
 * \return The current value of the unitlength
 */
int FltkDevice::UnitLength() const
{
    return this->unitLength;
}


/**
 * Changes the value the unitlength
 * \param NewUnitLength - Sets the current unitlength the NewUnitLength
 * \return The previous value of unitlentth
 */
int FltkDevice::UnitLength(int NewUnitLength)
{
    int OldUnitLength = this->unitLength;
    this->unitLength  = NewUnitLength;
    return OldUnitLength;
}
  
/**
 * Draws a pixel or a disd depending on the value of the current unitlength
 * \param x - the x-coordinate of the pixel
 * \param y - the y-coordinate of the pixel
 * \param red - The value of the red component ot the color
 * \param green - The value of the green component ot the color
 * \param blue - The value of the blue component ot the color
 */
void FltkDevice::SetPixel(int x, int y, double red, double green, double blue)
{
    //std::cout << "-->FltkDevice::SetPixel(int, int, double, double, double)" << std::endl;
    glColor3d(red, green, blue);
    if (this->unitLength > 1)
	Disk(x * this->unitLength, y * this->unitLength, 0.0,
	     (this->unitLength - 1) / 2, red, green, blue);
    else {
	glBegin(GL_POINTS);
	    glVertex2i(x, y);
	glEnd();
    }
    //std::cout << "<--FltkDevice::SetPixel(int, int, double, double, double)" << std::endl;
}

void FltkDevice::DrawGrid(int Xspacing, int Yspacing)
{
    glColor3d(0.0, 0.0, 0.0);
    int yspacing = this->unitLength * Yspacing;
    glBegin(GL_LINES);
        for (int y = 0; y < this->OrgHeight; y += yspacing) {
	    glVertex3d(0, double(y), 0.0);
	    glVertex3d(double(this->OrgWidth-1), double(y), 0.0);
	}
    glEnd();
    glColor3d(0.0, 0.0, 0.0);
    int xspacing = this->unitLength * Xspacing;
    glBegin(GL_LINES);
        for (int x = 0; x < this->OrgWidth; x += xspacing) {
	    glVertex3d(double(x), 0.0, 0.0);
	    glVertex3d(double(x), double(this->OrgHeight-1), 0.0);
	}
    glEnd();
}

/**
 * Draws the ideal line that the dots should approximate.
 * \param Xstart - the x-coordinate of the start point.
 * \param Ystart - the y-coordinate of the start point.
 * \param Xstop - the x-coordinate of the end point.
 * \param Ystop - the y-coordinate of the end point.
 * \param red - The value of the red component ot the color
 * \param green - The value of the green component ot the color
 * \param blue - The value of the blue component ot the color
 */
void FltkDevice::TestLine(int Xstart, int Ystart, int Xstop, int Ystop,
			  double red, double green, double blue)
{
  glColor3d(red, green, blue);
  glBegin(GL_LINES);
      glVertex2i(Xstart * this->unitLength, Ystart * this->unitLength);
      glVertex2i(Xstop  * this->unitLength, Ystop  * this->unitLength);
  glEnd();
}

/**
 * Draws a circle in the window
 * \param Xcenter - The x-coordinate of the center of the circle
 * \param Ycenter - The y-coordinate of the center of the circle
 * \param Zcenter - The z-coordinate of the center of the circle
 * \param Radius - The radius of the circle
 * \param red - The value of the red component ot the color
 * \param green - The value of the green component ot the color
 * \param blue - The value of the blue component ot the color
 */
void FltkDevice::Circle(int Xcenter, int Ycenter, double Zcenter, int Radius,
			double red, double green, double blue)
{
    int X = 0;
    int Y = Radius;
    int d = 1 - Radius;
    int deltaE  = 3;
    int deltaSE = -2 * Radius + 5;
    
    glColor3d(red, green, blue);
    glBegin(GL_POINTS);
        CirclePoints(Xcenter, Ycenter, Zcenter, X, Y);
  
	while (Y > X) {
	    if (d < 0) {
		d += deltaE;
		deltaSE += 2;
	    }
	    else {
		d += deltaSE;
		deltaSE += 4;
		--Y;
	    }
	    ++X;
	    deltaE += 2;
	    CirclePoints(Xcenter, Ycenter, Zcenter, X, Y);
	}
    glEnd();
}


/**
 * Draws a disk - a filled circle in the window
 * \param Xcenter - The x-coordinate of the center of the disk
 * \param Ycenter - The y-coordinate of the center of the disk
 * \param Zcenter - The z-coordinate of the center of the disk
 * \param Radius - The radius of the disk
 * \param red - The value of the red component ot the color
 * \param green - The value of the green component ot the color
 * \param blue - The value of the blue component ot the color
 */
void FltkDevice::Disk(int Xcenter, int Ycenter, double Zcenter, int Radius,
		      double red, double green, double blue)
{
    int X = 0;
    int Y = Radius;
    int d = 1 - Radius;
    int deltaE  = 3;
    int deltaSE = -2 * Radius + 5;
  
    glColor3d(red, green, blue);
    glBegin(GL_LINES);
        CirclePoints(Xcenter, Ycenter, Zcenter, X, Y);
  
	while (Y > X) {
	    if (d < 0) {
		d += deltaE;
		deltaSE += 2;
	    }
	    else {
		d += deltaSE;
		deltaSE += 4;
		--Y;
	    }
	    ++X;
	    deltaE += 2;
	    this->CirclePoints(Xcenter, Ycenter, Zcenter, X, Y);
	}
    glEnd();
}

/**
 * Defines the start point and the end point on a scanline given a point on a circle
 * It uses symmetri so it is only necessary to scanconvert one eight of a circle arch
 */
void FltkDevice::CirclePoints(int Xcenter, int Ycenter, double Zcenter, int X, int Y)
{
    glVertex3d(double(Xcenter - X), double(Ycenter - Y), Zcenter);
    glVertex3d(double(Xcenter + X), double(Ycenter - Y), Zcenter);
    glVertex3d(double(Xcenter - X), double(Ycenter + Y), Zcenter);
    glVertex3d(double(Xcenter + X), double(Ycenter + Y), Zcenter);
    glVertex3d(double(Xcenter - Y), double(Ycenter - X), Zcenter);
    glVertex3d(double(Xcenter + Y), double(Ycenter - X), Zcenter);
    glVertex3d(double(Xcenter - Y), double(Ycenter + X), Zcenter);
    glVertex3d(double(Xcenter + Y), double(Ycenter + X), Zcenter);
}
