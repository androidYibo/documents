CXX=g++

CXXFLAGS=-trigraphs -Wno-deprecated -I. -I/usr/local/include -I/usr/local/include/FL/images -I/usr/include  -I${HOME}/Documents/Include -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_THREAD_SAFE -D_REENTRANT -Wfatal-errors

LDFLAGS=-L/usr/local/lib -lGL -lGLEW -lglut -lGLU -lm

PGM=testglm testbeziercurve mouseBezier

OBJ=glmutils.o beziercurve.o mouseBezier.o testbeziercurve.o testglm.o

# all: testglm testbeziercurve mouseBezier
all: mouseBezier
obj: ${OBJ}

# glmutils.o: glmutils.h glmutils.cpp
	# ${CXX} ${CXXFLAGS} -c glmutils.cpp

# beziercurve.o: beziercurve.h beziercurve.cpp glmutils.h
	# ${CXX} ${CXXFLAGS} -c beziercurve.cpp

# testglm.o: testglm.cpp
	# ${CXX} ${CXXFLAGS} -c testglm.cpp

# testglm: testglm.o glmutils.o
	# ${CXX} ${CXXFLAGS} -o testglm testglm.o glmutils.o ${LDFLAGS}

# testbeziercurve.o: testbeziercurve.cpp beziercurve.h beziercurve.cpp glmutils.h glmutils.cpp
	# ${CXX} ${CXXFLAGS} -c testbeziercurve.cpp

# testbeziercurve: beziercurve.o glmutils.o testbeziercurve.o
	# ${CXX} ${CXXFLAGS} -o testbeziercurve testbeziercurve.o beziercurve.o glmutils.o ${LDFLAGS}

mouseBezier.o: mouseBezier.cpp
	${CXX} ${CXXFLAGS} -c mouseBezier.cpp

mouseBezier: mouseBezier.o
	${CXX} ${CXXFLAGS} -o mouseBezier mouseBezier.o ${LDFLAGS}

clean:
	rm -f ${PGM} ${OBJ}
