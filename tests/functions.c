/*  
    test C functions.
 */

int plus(int a, int b) 
{
  return a+b;
}

int callback_proc(int (*f)(int), int n)
{
  return f(n);
}

extern int externed_variable;
int externed_variable = 10;

int get_externed_variable()
{
  return externed_variable;
}

/* TODO more */
