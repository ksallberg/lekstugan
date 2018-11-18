UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S), Linux)
    comp := gcc
    flags := -L /usr/local/lib/ -lGLEW -lglfw -lGL -lX11 -lXi -lXrandr -lXxf86vm -lXinerama -lXcursor -lrt -lm -pthread -ldl
endif

ifeq ($(UNAME_S), Darwin)
    comp := clang
    flags := -lglfw -framework CoreVideo -framework OpenGL -framework IOKit -framework Cocoa -framework Carbon
endif


build:
	$(comp) fireworks.c $(flags)

clean:
	rm -f a.out

run:
	./a.out

mem:
	valgrind --leak-check=yes ./a.out
