#include "LedControl.h"
#include <Servo.h>

// servo:
// svarta ar jord
// roda ar power (3 volt)

// display:
// pin 12 is connected to the DataIn (DIN)
// pin 11 is connected to the CLK (left side CLK)
// pin 10 is connected to LOAD
// left vcc is connected to power (5 volt)

LedControl lc=LedControl(12,11,10,1);

Servo myservo;
int high = 28800;
//int high = 5;
int low = 20;
//int low = 4;
int pos = 0;
int servopin = 2;
unsigned long delaytime=1000;
int counter=high;
int state = 0;

void setup() {

  lc.shutdown(0,false);
  lc.setIntensity(1,80);
  lc.clearDisplay(0);
  myservo.write(180);
  myservo.attach(servopin);
}

void loop() {

    int digit = counter;
    int dig1 = digit % 10;
    digit /= 10;
    int dig2 = digit % 10;
    digit /= 10;
    int dig3 = digit % 10;
    digit /= 10;
    int dig4 = digit % 10;
    digit /= 10;
    int dig5 = digit % 10;

    lc.clearDisplay(0);

    //lc.setDigit(0,7,0,false);
    //lc.setDigit(0,6,0,false);
    //lc.setDigit(0,5,0,false);
    lc.setDigit(0,4,dig5,false);
    lc.setDigit(0,3,dig4,false);
    lc.setDigit(0,2,dig3,false);
    lc.setDigit(0,1,dig2,false);
    lc.setDigit(0,0,dig1,false);

    // up
    if(state == 0) {
      if(counter > 0) {
        counter --;
      } else {
        pos=100;
        myservo.attach(servopin);
        myservo.write(160);
        delay(200);
        myservo.write(140);
        delay(200);
        myservo.write(120);
        delay(200);
        myservo.write(pos);
        delay(200);
        myservo.detach();
        state = 1;
        counter = low;
      }
    // down
    } else if(state ==1) {
      if(counter > 0) {
        counter --;
      } else {
        pos=180;
        myservo.attach(servopin);
        myservo.write(pos);
        delay(500);
        myservo.detach();
        state = 0;
        counter = high;
      }
    }
    delay(delaytime);
}
