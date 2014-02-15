CXXFLAGS=-trigraphs -Wno-deprecated -I. -I/usr/local/include -I/usr/local/include/FL/images -I/usr/include  -I${HOME}/Documents/Include -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_THREAD_SAFE -D_REENTRANT

LDFLAGS=-L/usr/local/lib -lfltk_gl -lfltk -lpthread -lm -L/usr/lib -lGLEW -lGL -lGLU

all: fltkcontrolpanel.o testfltkdotdevice 

fltkcontrolpanel.o: fltkcontrolpanel.h fltkcontrolpanel.cpp
	g++ ${CXXFLAGS} -c fltkcontrolpanel.cpp

fltkdotdevice.o: fltkdotdevice.h fltkdotdevice.cpp
	g++ ${CXXFLAGS} -c fltkdotdevice.cpp

testfltkdotdevice.o: testfltkdotdevice.cpp fltkdotdevice.h
	g++ ${CXXFLAGS} -c testfltkdotdevice.cpp

testfltkdotdevice: testfltkdotdevice.o fltkdotdevice.o fltkcontrolpanel.o
	g++ ${CXXFLAGS}  -o testfltkdotdevice fltkcontrolpanel.o fltkdotdevice.o testfltkdotdevice.o ${LDFLAGS}



clean:
