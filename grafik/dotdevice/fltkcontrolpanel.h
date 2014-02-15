#ifndef FLTKCONTROLPANEL_H
#define FLTKCONTROLPANEL_H

#include <FL/Fl.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Button.H>

#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <cmath>
#include <string>
#include <sstream>
#include <cctype>


class FltkDevice;


/**
 * \class FltkControlPanel
 * A class which impelments an array of buttens which controls the actions of an FltkDevice
 */

/**
 * This class implements an array of buttons that call individual member functions 
 * of an FltkDevice
 */
class FltkControlPanel : public Fl_Group {
public:
    /**
     * \typedef ActionFunction
     * A pointer to a member function in the class FltkDevice. It takes no parameters
     */
    typedef void (FltkDevice::*ActionFunction)(void);

    /**
     * \struct ButtonData
     * The data needed for each button in a button-array
     */
    typedef struct { 
	/*@{*/
	Fl_Button*     Button;   //!< A pointer to the button
	std::string    Name;     //!< The name on the button
	long           ButtonId; //!< The Id of the button, numbered from 1
	std::string    ToolTip;  //!< The text used for tooltips
	ActionFunction Action;   //!< The address of the member function to be called 
	/*@}*/
    } ButtonData;

    /**
     * Parameterized constructor, creates a window containing an array of buttons 
     * which controls an FltkDevice
     * \param X - the x-coordinate of the window position
     * \param Y - the y-coordinate of the window position
     * \param Width - the width of the window
     * \param Height - the height of the window
     * \param Title - the window title
     * \param Device - a pointer to the device which handles the button call-backs
     * \param Rows - the number of rows in the button-array
     * \param Columns - the number of columns in the button-array
     * \param BData - an array of data with one entry for each button in the button-array
     * \param BDentries - the number of entries in the BData array
     */
    FltkControlPanel(int const X, int const Y, int const Width, int const Height,
		     std::string const& Title,
		     FltkDevice* Device,
		     int const Rows, int const Columns, 
		     ButtonData const BData[], int const BDentries);
    
    /**
     * Destructor, cleans up the internal data structures and closes the window
     */
    virtual ~FltkControlPanel();

protected:
    
private:
    /**
     * Dispatches the callback so the right private function is called.
     * This is done by calling a local non-static function which has access to
     * all the private variables.
     * The type of this callback is Fl_Callback1.
     * The function is static because that is the way Fltk needs it.
     * The function is static.
     * \param Button - a pointer to the button pressed
     * \param Device - a pointer to the device owning this controlpanel
     */
    static void Dispatch(Fl_Widget* Button, long ButtonId);

    /**
     * Dispatches button events
     * \param ButtonId - the id of the button that was pressed
     */
    void Dispatch(long ButtonId);

    /**
     * Private Variables
     */

    /**
     * Stores the FltkDevice which has the FltkControlPanel attached to it
     */
    FltkDevice* Device;

    /**
     * The number of rows in the button-array
     */
    int Nrows;

    /**
     * The number of columes in the button-array
     */
    int Ncols;
    
    /**
     * The number of entries in the Buttons array = Nrows * Ncols
     */
    int ButtonEntries;

    /**
     * An array which holds the buttons in the arrray
     */
    ButtonData* Buttons;
};

#endif
