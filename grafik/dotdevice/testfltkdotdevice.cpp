#include <GL/glew.h>
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/fl_ask.H>


#include "fltkdotdevice.h"
#include "fltkcontrolpanel.h"


/**
 * Global Variables
 */

Fl_Window*        GLwindow      = 0;
FltkDevice*       Device        = 0;
Fl_Window*        ControlWindow = 0;
FltkControlPanel* ControlPanel  = 0;



void printBData(FltkControlPanel::ButtonData BData[], int BDentries)
{
    for (int Bindex = 0; Bindex < BDentries; ++Bindex) {
	std::cout << "BData[" << Bindex + 1 << "]: " << std::endl;
	std::cout << "         Button:   " << BData[Bindex].Button   << std::endl;
	std::cout << "         Name:     " << BData[Bindex].Name     << std::endl;
	std::cout << "         ButtonId: " << BData[Bindex].ButtonId << std::endl;
	std::cout << "         ToolTip:  " << BData[Bindex].ToolTip  << std::endl;
	std::cout << "         Action:   " << BData[Bindex].Action   << std::endl;
    }
}


int main(int argc, char** argv)
{
    int result = 0;
    
    // define the width and height of buttons, so everything can be automated, especially the window size
    int const xpos    = 200;   // X position of the FltkDevice
    int const ypos    = 500;   // Y position of the FltkDevice
    int const width   = 800;   // Width of the FltkDevice window
    int const height  = 500;   // Height of the FltkDevice window

    int const brows   = 3;     // Number of rows in the FltkControlPanel
    int const bcols   = 3;     // Number of columns in the FltkControlPanel
    int const bwidth  = 150;   // Width of a button
    int const bheight = 50;    // Height of a button

    int const cxpos   = xpos + width + 50;   // X position of the FltkControlPanel
    int const cypos   = ypos;                // Y position of the FltkControlPanel
    int const cwidth  = bcols * bwidth;      // Width of the FltkControlPanel
    int const cheight = brows * bheight;     // Height of the FltkControlPanel
    
    try {
	// Wrap the FltkDevice in a Fl_Window
	GLwindow = new Fl_Window(xpos, ypos, width, height, "Fltk Window");
	GLwindow->begin();
	    Device   = new FltkDevice(0, 0, width, height, std::string("Fltk Window"));
	GLwindow->end();
	//GLwindow->resizable(Device);
        GLwindow->show();
	// Setup the buttons of an array for an FltkControlPanel
	FltkControlPanel::ButtonData BData[] = { 
	    { 0, "CallBack 1",   1, "CallBack 1", &FltkDevice::Callback_1 },
	    { 0, "CallBack 2",   2, "CallBack 2", &FltkDevice::Callback_2 },
	    { 0, "CallBack 3",   3, "CallBack 3", &FltkDevice::Callback_3 },
	    { 0, "CallBack 4",   4, "CallBack 4", &FltkDevice::Callback_4 },
	    { 0, "CallBack 5",   5, "CallBack 5", &FltkDevice::Callback_5 },
	    { 0, "CallBack 6",   6, "CallBack 6", &FltkDevice::Callback_6 },
	    { 0, "CallBack 7",   7, "CallBack 7", &FltkDevice::Callback_7 },
	    { 0, "CallBack 8",   8, "CallBack 8", &FltkDevice::Callback_8 },
	    { 0, "CallBack 9",   9, "CallBack 9", &FltkDevice::Callback_9 }
	};
	int BDentries = sizeof(BData) / sizeof(FltkControlPanel::ButtonData);

	// Make an FltKControlPanel
	std::cout << "Mading ControlWindow" << std::endl;
	ControlWindow = new Fl_Window(cxpos, cypos, cwidth, cheight, "FltkControlPanel");
	ControlWindow->begin();
	    ControlPanel = new FltkControlPanel(0, 0, cwidth, cheight, "FltkControlPanel",
	 					Device, brows, bcols, BData, BDentries);
	ControlWindow->end();
	ControlWindow->show();

	// Go into the event loop
	Fl::run();
    }
    catch (std::exception const& Exception) {
	std::cout << "Exception: " << Exception.what() << std::endl;
	exit(1);
    }

    return result;
}
