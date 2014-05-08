#!/bin/sh
g++ -I /usr/local/include -c $1.cpp
g++ -v -o $1 -L /usr/local/lib $1.o -lgecodedriver -lgecodesearch -lgecodeint -lgecodekernel -lgecodesupport -lgecodeminimodel #-lgecodegist
rm -rf $1.o
