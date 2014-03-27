#include <iostream>
#include <iomanip>
#include <stdexcept>
#include <string>
#include <sstream>

#include <FL/fl_ask.H>
#include "fltkcontrolpanel.h"


FltkControlPanel::FltkControlPanel(int const X, int const Y, int const Width, int const Height,
				   std::string const& Title,
				   FltkDevice* Device,
				   int const Rows, int const Columns,
				   ButtonData const BData[], int const BDentries)
                : Fl_Group(X, Y, Width, Height, Title.c_str()), 
		  Device(Device), Nrows(Rows), Ncols(Columns), 
		  Buttons(new ButtonData[Rows * Columns]), ButtonEntries(BDentries)
{
    if (Rows * Columns < BDentries) {
	std::ostringstream errormessage;
	errormessage << "FltkControlPanel::FltkControlPanel(...): BData has too few intries. ";
	errormessage << "Should have had " << Rows * Columns << " entries";
	throw std::runtime_error(errormessage.str());
    }

    int ButtonIndex = 0;
    int xpos   = 0;
    int ypos   = 0;
    int width  = 150;
    int height = 50;
    for (int row = 1; row <= Rows; ++row) {
	xpos = 0;
	for (int col = 1; col <= Columns; ++col) {
	    // Copy BData[ButtonIndex] to the private variable Buttons[ButtonIndex]
	    this->Buttons[ButtonIndex].Button   = BData[ButtonIndex].Button;
	    this->Buttons[ButtonIndex].Name     = BData[ButtonIndex].Name;
	    this->Buttons[ButtonIndex].ButtonId = BData[ButtonIndex].ButtonId;
	    this->Buttons[ButtonIndex].ToolTip  = BData[ButtonIndex].ToolTip;
	    this->Buttons[ButtonIndex].Action   = BData[ButtonIndex].Action;

	    // Setup the buttonarray
	    Fl_Button* newButton = new Fl_Button(xpos, ypos, width, height, 
						 this->Buttons[ButtonIndex].Name.c_str());
	    newButton->down_box(FL_DOWN_BOX);
	    newButton->labeltype(FL_ENGRAVED_LABEL);
	    newButton->tooltip(this->Buttons[ButtonIndex].ToolTip.c_str());
	    newButton->callback(this->Dispatch, this->Buttons[ButtonIndex].ButtonId);
	    this->Buttons[ButtonIndex].Button = newButton;
	    
	    xpos += width;
	    ++ButtonIndex;
	}
	ypos += height;
    }
}

FltkControlPanel::~FltkControlPanel()
{}

/**
 * Protected functions
 */


/**
 * Private functions
 */

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
void FltkControlPanel::Dispatch(Fl_Widget* Button, long ButtonId)
{
    //std::cout << "-->FltkControlPanel::Dispatch(Fl_Widget*, long)" << std::endl;
    //std::cout << "   Button pressed: " << ButtonId << std::endl;
    dynamic_cast<FltkControlPanel*>(Button->parent())->Dispatch(ButtonId);
    
    //std::cout << "<--FltkControlPanel::Dispatch(Fl_Widget*, long)" << std::endl;
}

/**
 * Dispatches button events
 * \param ButtonId - the id of the button that was pressed
 */
void FltkControlPanel::Dispatch(long ButtonId)
{
    //std::cout << "   -->FltkControlPanel::Dispatch(long)" << std::endl;
    //std::cout << "      ButtonId = " << ButtonId << std::endl;

    if (ButtonId > (this->Nrows * this->Ncols)) {
	std::ostringstream errormessage;
	errormessage << "FltkControlPanel::Dispatch(long): There is no ButtonId = "
		     << ButtonId << std::endl;
	throw std::runtime_error(errormessage.str());
    }

    int Bindex = ButtonId - 1;

    if (this->Buttons[Bindex].Action == 0) {
	std::ostringstream errormessage;
	errormessage << "FltkControlPanel::Dispatch(long): Button "
		     << this->Buttons[Bindex].Name << " has no callback function" << std::endl;
	fl_alert("%s", errormessage.str().c_str());
    }
    else {
	((this->Device)->*Buttons[Bindex].Action)();
    }

    //std::cout << "   <--FltkControlPanel::Dispatch(long)" << std::endl;
}
