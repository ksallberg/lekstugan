#include <stdio.h>

int main() {
  char *text = "hej hej";
  int len = strlen(text) + 2;
  int padding = 1;
  // skriv ut topp
  printf(" ");
  for(int i = 0; i < len; i ++) {
    printf("*");
  }
  printf("\n");
  // skriv ut rad med text
  printf("* %s *\n", text);

  while(len > 0) {
    // skriv ut rad under text
    for(int i = 0; i < padding; i ++) {
      printf(" ");
    }
    padding ++;
    for(int i = 0; i < len; i ++) {
      printf("*");
    }
    printf("\n");
    len -= 2;
  }
}
