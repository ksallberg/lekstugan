#include <Servo.h>

Servo myservo;

int pos = 0;
int servopin = 2;
int dir=90;

void setup() {
  myservo.attach(servopin);
}

void loop() {
  for(pos = 0; pos <= dir; pos ++) {
    myservo.write(pos);
    delay(15);
  }
  delay(1500);
  for(pos = dir; pos >= 0; pos --) {
    myservo.write(pos);
    delay(15);
  }
  delay(1500);
}
