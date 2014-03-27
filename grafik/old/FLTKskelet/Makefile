CXX=g++

CXXFLAGS=-trigraphs -Wno-deprecated -I. -I/usr/local/include -I/usr/local/include/FL/images -I/usr/include  -I${HOME}/Documents/Include -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_THREAD_SAFE -D_REENTRANT

LDFLAGS=-L/usr/local/lib -lfltk_gl -lfltk -lpthread -lm -L/usr/lib -lGLEW -lGL -lGLU


all: GLTriangle.o ShaderProgram.o fltkcontrolpanel.o fltkdevice.o testfltkdevice

GLTriangle.o: GLTriangle.h GLTriangle.cpp
	${CXX} ${CXXFLAGS} -c GLTriangle.cpp

ShaderProgram.o: ShaderProgram.h ShaderProgram.cpp
	${CXX} ${CXXFLAGS} -c ShaderProgram.cpp

fltkcontrolpanel.o: fltkcontrolpanel.h fltkcontrolpanel.cpp
	${CXX} ${CXXFLAGS} -c fltkcontrolpanel.cpp

fltkdevice.o: fltkdevice.h fltkdevice.cpp GLTriangle.h ShaderProgram.h
	${CXX} ${CXXFLAGS} -c fltkdevice.cpp

testfltkdevice.o: testfltkdevice.cpp fltkdevice.h GLTriangle.h ShaderProgram.h
	${CXX} ${CXXFLAGS} -c testfltkdevice.cpp

testfltkdevice: testfltkdevice.o GLTriangle.o ShaderProgram.o fltkdevice.o fltkcontrolpanel.o
	${CXX} ${CXXFLAGS}  -o testfltkdevice GLTriangle.o ShaderProgram.o fltkcontrolpanel.o fltkdevice.o testfltkdevice.o ${LDFLAGS}

clean:
	rm -f testfltdevice *.o
