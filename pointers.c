int main() {
  char *str1 = "det";
  char *str2 = "var";
  char *str3 = "en";
  char *str4 = "gang";
  char *str5 = ".";

  char **sentence = {str1, str2, str3, str4, str5};

  char **x = sentence;

  printf("%s", str1);
  printf("%s", str2);
  printf("%s", str3);

  printf("\n");

  printf(&sentence[0]);
  printf(&sentence[1]);

  printf("\n___");

  printf(x);

  printf("\n");
}
