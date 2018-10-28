UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S), Linux)
    comp := gcc
    flags := -L /usr/local/lib/ -lGLEW -lglfw3 -lGL -lX11 -lXi -lXrandr -lXxf86vm -lXinerama -lXcursor -lrt -lm -pthread -ldl
endif

ifeq ($(UNAME_S), Darwin)
    comp := clang
    flags := -lglfw -framework CoreVideo -framework OpenGL -framework IOKit -framework Cocoa -framework Carbon fireworks.c
endif


build:
	$(comp) fireworks.c $(flags)

clean:
	rm -f a.out

run:
	./a.out
