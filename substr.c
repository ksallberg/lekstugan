ar word;
char *string;

while( *string != '\0' ) {

      char *to;
      to = strndup( string, 1 );

      printf( "%s, \n", to );
      string ++;
}
