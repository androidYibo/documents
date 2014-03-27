CXXFLAGS=-trigraphs -Wno-deprecated -I. -I/usr/local/include -I/usr/local/include/FL/images -I/usr/include  -I${HOME}/Documents/Include -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_THREAD_SAFE -D_REENTRANT

LDFLAGS=-L/usr/local/lib -lGL -lGLEW -lglut -lm

all: Run 

GLCurve.o: GLCurve.h GLCurve.cpp
	g++ ${CXXFLAGS} -c GLCurve.cpp

ShaderProgram.o: ShaderProgram.h ShaderProgram.cpp
	g++ ${CXXFLAGS} -c ShaderProgram.cpp

main.o: main.cpp
	g++ ${CXXFLAGS} -c main.cpp

Run: main.o GLCurve.o ShaderProgram.o
	g++ ${CXXFLAGS} -o Run GLCurve.o ShaderProgram.o main.o ${LDFLAGS}

clean:
	rm -f Run *.o
