#ifndef RASTERMAN_FLTK_DOT_DEVICE_H
#define RASTERMAN_FLTK_DOT_DEVICE_H

#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <cctype>

#include <FL/Fl_Gl_Window.H>

#include "fltkcontrolpanel.h"
#include "glm/glm.hpp"
#include "glm/gtc/matrix_transform.hpp"
#include "glm/gtc/type_ptr.hpp"

#include "GLTriangle.h"
#include "ShaderProgram.h"


/**
 * \class FltkDevice
 * A class which implements an OpgeGL window using the toolkit Fltk
 */ 
class FltkDevice : public Fl_Gl_Window {
public:
    /**
     * \typedef ButtonID
     * The names of the valid buttons which can be pressed
     */
    typedef enum { 
	/*@{*/
	Button_0,   //!< Illegal button
	Button_1,   //!< Draw something
	Button_2,   //!< Draw something  
	Button_3,   //!< Draw something
	Button_4,   //!< Draw something
	Button_5,   //!< Draw something
	Button_6,   //!< Draw something
	Button_7,   //!< Draw something
	Button_8,   //!< Draw something
	Button_9    //!< Draw something
	/*@}*/
    } ButtonID;

    /**
     * Parameterized constructor, creates a window which can be used for drawing OpenGL graphics
     * \param X - the x-coordinate of the window position
     * \param Y - the y-coordinates of the window position
     * \param Width - the width of the window
     * \param Height - the height of the window
     * \param Title - the window title
     */
    FltkDevice(int const X, int const Y, int const Width, int const Height, std::string const& Title);

    /**
     * Destructor, cleans up the internal data structures and closes the window
     */
    virtual ~FltkDevice();

    /**
     * Clears the window
     */
    void Clear();

    /**
     * Reset the internal data structures to default values
     */
    void Reset();

    /**
     * Clears the screen and redraws the contents of the window
     */
    void Update();

    /**
     * A callback function
     */
    void Callback_1();

    /**
     * A callback function
     */
    void Callback_2();

    /**
     * A callback function
     */
    void Callback_3();

    /**
     * A callback function
     */
    void Callback_4();

    /**
     * A callback function
     */
    void Callback_5();

    /**
     * A callback function
     */
    void Callback_6();

    /**
     * A callback function
     */
    void Callback_7();

    /**
     * A callback function
     */
    void Callback_8();

    /**
     * A callback function
     */
    void Callback_9();


protected:

private:
    /**
     * Renders the shaped known to the device at the moment.
     * This is where all the drawing of geometric shapes id done.
     */
    void RenderShapes();

    /**
     * Initializes the OpgnGL context
     */
    virtual void InitializeGL();
    
    /**
     * Initializes the viewport so it is centered in the window
     */
    virtual void InitializeViewport(int Width, int Height);

    /**
     * Initializes the GLEW library
     */
    virtual void InitializeGLEW();

    /**
     * Initializes the Shader Programs
     */
    void InitializeShadersAndGLObjects();

    /**
     * Checks if any errors occurred during processing of OpenGL requests.
     * If an error has occurred an exception is thown
     */
    void ErrorCheck() const;

    /**
     * Physically clears the window
     */
    void clear();

    /**
     * Cleans up the internal data structures
     */
    virtual int CleanUp();

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
    void resize(int X, int Y, int Width, int Height);

    /**
     * Does the actual drawing of the contents of the window using OpenGL commands
     */
    void draw();

    /**
     * Handles all events which occur while the window is in focus
     * \param Event - the Id of the event
     */
    int  handle(int Event);

    /**
     * Handles a key press event
     */
    virtual int HandleShortCutEvent(int const Key);

    /**
     * Cleans up and exits
     */
    int Quit();


    /**
       Private Variables
    */

    /**
     * The original x-coordinate of lower left corner of the viewport
     */
    int Xorg;

    /**
     * The original y-coordinate of lower left corner of the viewport
     */
    int Yorg;
    /**
     * The original width of the viewport
     */
    int OrgWidth;

    /**
     * The original height of the viewport
     */
    int OrgHeight;

    /**
     * The original aspect ratio of the window
     */
    float AspectRatio;

    /**
     * true if the library GLEW has been initialized, false otherwize
     */
    bool glewInitialized;

    /**
     * true if a clear request has been issued, false otherwise. 
     * If it is true the window will be cleared next time the function draw is called
     */
    bool pendingClear;

    /**
     * true if an update request has been issued, false otherwise.
     * If it is true the window will be cleared and updated next time the function 
     * draw is called.
     */
    bool pendingUpdate;

    /**
     * A Class which can draw triangles.
     */
    GLTriangle triangle;

    /**
     * A Class which can handle shader programs.
     */
    ShaderProgram triangleShaderProgram;

    /**
     * Contains the ID of the current shape, i.e. the shape which will be drawn on the next 
     * call to Update().
     */
    ButtonID CurrentButton;
};

#endif
