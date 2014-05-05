CXXFLAGS=-trigraphs -Wno-deprecated -I. -I/usr/local/include -I/usr/local/include/FL/images -I/usr/include  -I${HOME}/Documents/Include -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_THREAD_SAFE -D_REENTRANT

LDFLAGS=-L/usr/local/lib -lGL -lGLEW -lglut -lm

all: Run 

# glmutils.o: glmutils.h glmutils.cpp
	# g++ ${CXXFLAGS} -c glmutils.cpp

# testglm.o: testglm.h testglm.cpp
	# g++ ${CXXFLAGS} -c testglm.cpp

bezierpatch.o: bezierpatch.h bezierpatch.cpp
	g++ ${CXXFLAGS} -c bezierpatch.cpp

readbezierpatches.o: readbezierpatches.h readbezierpatches.cpp
	g++ ${CXXFLAGS} -c readbezierpatches.cpp

ShaderProgram.o: ShaderProgram.h ShaderProgram.cpp
	g++ ${CXXFLAGS} -c ShaderProgram.cpp

main.o: main.cpp
	g++ ${CXXFLAGS} -c main.cpp

GLPatch.o: GLPatch.h GLPatch.cpp
	g++ ${CXXFLAGS} -c GLPatch.cpp

# Run: main.o GLPatch.o ShaderProgram.o bezierpatch.o glmutils.o readbezierpatches.o 
	# g++ ${CXXFLAGS} -o Run GLPatch.o ShaderProgram.o main.o bezierpatch.o glmutils.o ${LDFLAGS}

Run: main.o GLPatch.o ShaderProgram.o bezierpatch.o readbezierpatches.o 
	g++ ${CXXFLAGS} -o Run GLPatch.o ShaderProgram.o main.o bezierpatch.o readbezierpatches.o ${LDFLAGS}

clean:
	rm -f Run *.o
