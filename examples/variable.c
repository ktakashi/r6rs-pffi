#include <stdlib.h>

int global_int = 10;
char *global_string = NULL;

int get_global_int()
{
  return global_int;
}

void init_global_string()
{
  static const char hello[] = "hello";
  int i;
  size_t len = sizeof(hello)/sizeof(hello[0]);

  global_string = (char *)malloc(len + 1);
  for (i = 0; i < len; i++) {
    global_string[i] = hello[i];
  }
  global_string[i] = '\0';
}

char * get_global_string()
{
  return global_string;
}


/* gcc -fPIC -shared -o variable.so variable.c */
