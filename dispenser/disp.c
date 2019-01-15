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
int pos = 0;
int servopin = 2;
unsigned long delaytime=250;
int i=0;
void setup() {

  lc.shutdown(0,false);
  lc.setIntensity(1,80);
  lc.clearDisplay(0);
  myservo.attach(servopin);
}

void loop() {

    if(i == 8) {
      i = 0;
    }
    lc.clearDisplay(0);

    lc.setDigit(0,3,i,false);
    lc.setDigit(0,2,i,false);
    lc.setDigit(0,1,i,false);
    lc.setDigit(0,0,i,false);

    lc.setDigit(0,4,i+1,false);
    lc.setDigit(0,5,i+1,false);
    lc.setDigit(0,6,i+1,false);
    lc.setDigit(0,7,i+1,false);

    i++;
    if(i==3) {
       pos=0;
       myservo.write(pos);
    }
    if(i==7) {
       pos=0;
       myservo.write(pos);
    }
    delay(delaytime);
}
