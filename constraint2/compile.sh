#!/bin/ksh
#g++ -I/usr/local/include -I/usr/include -I/usr/include/qt5/QtGui -I/usr/include/qt5/QtCore -c $1.cpp

g++ -I/usr/local/include -I/usr/include/qt5 -I/usr/include/qt5/QtWidgets -I/usr/include/qt5/QtGui -c $1.cpp -fPIC

g++ -o app $1.o -L/usr/lib -L/usr/local/lib -lQt5Core -lQt5Widgets -lQt5Gui \
    -lgecodedriver -lgecodegist -lgecodesearch -lgecodeminimodel \
    -lgecodeint -lgecodekernel -lgecodesupport
