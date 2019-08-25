AR ?= ar

SRCS = list.c \
		   list_node.c \
		   list_iterator.c

OBJS = $(SRCS:.c=.o)

%.o: %.c
	$(CC) $< $(CFLAGS) -c -o $@

build/liblist.a: $(OBJS)
	@mkdir -p build
	$(AR) rcs $@ $^



all:
	git -C list pull || git clone https://github.com/clibs/list.git
	make -C list/
	gcc -w -o ws list/build/liblist.a wordsnake.c
	g++ -o cppsnake wordsnake.cpp
	javac wordsnake.java

run:
	./ws
