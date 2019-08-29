int main() {
  char *str1 = "det";
  char *str2 = "var";
  char *str3 = "en";
  char *str4 = "gang";

  char **sentence[10];//{str1, str2, str3, str4, str5};
  sentence[0] = str1;
  sentence[1] = str2;
  sentence[2] = str3;
  sentence[3] = str4;

  char **x = sentence;

  for(int i = 0; i < 4; i ++) {
    printf("%s ", *x);
    x++;
  }

  printf("\n");
}
