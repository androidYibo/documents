CXX=g++

CXXFLAGS=-trigraphs -Wno-deprecated -I. -I/usr/local/include -I/usr/local/include/FL/images -I/usr/include  -I${HOME}/Documents/Include -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_THREAD_SAFE -D_REENTRANT

# LDFLAGS=-L/usr/local/lib -lfltk_gl -lfltk -lpthread -lm
LDFLAGS=-L/usr/local/lib -lfltk_gl -lfltk -lpthread -lm -L/usr/lib -lGLEW -lGL -lGLU
# LDFLAGS=-L/usr/local/lib -lfltk_gl -lfltk -framework AGL -framework Cocoa -L/usr/lib -lGLEW -framework OpenGL -framework GLUT -lpthread -lm

PGM=testglm testbeziercurve

OBJ=glmutils.o beziercurve.o

# all: testglm testbeziercurve
all: fltkcontrolpanel.o testfltkdotdevice 

obj: ${OBJ}

glmutils.o: glmutils.h glmutils.cpp
	${CXX} ${CXXFLAGS} -c glmutils.cpp

beziercurve.o: beziercurve.h beziercurve.cpp glmutils.h
	${CXX} ${CXXFLAGS} -c beziercurve.cpp

testglm.o: testglm.cpp
	${CXX} ${CXXFLAGS} -c testglm.cpp

testglm: testglm.o glmutils.o 
	${CXX} ${CXXFLAGS} -o testglm testglm.o glmutils.o ${LDFLAGS}

testbeziercurve.o: testbeziercurve.cpp beziercurve.h beziercurve.cpp glmutils.h glmutils.cpp
	${CXX} ${CXXFLAGS} -c testbeziercurve.cpp

testbeziercurve: beziercurve.o glmutils.o testbeziercurve.o
	${CXX} ${CXXFLAGS} -o testbeziercurve testbeziercurve.o beziercurve.o glmutils.o ${LDFLAGS}

fltkcontrolpanel.o: fltkcontrolpanel.h fltkcontrolpanel.cpp
	g++ ${CXXFLAGS} -c fltkcontrolpanel.cpp

fltkdotdevice.o: fltkdotdevice.h fltkdotdevice.cpp
	g++ ${CXXFLAGS} -c fltkdotdevice.cpp

testfltkdotdevice.o: testfltkdotdevice.cpp fltkdotdevice.h
	g++ ${CXXFLAGS} -c testfltkdotdevice.cpp

testfltkdotdevice: testfltkdotdevice.o fltkdotdevice.o fltkcontrolpanel.o
	g++ ${CXXFLAGS}  -o testfltkdotdevice fltkcontrolpanel.o fltkdotdevice.o testfltkdotdevice.o ${LDFLAGS}

clean:
	rm -f ${PGM} ${OBJ}
