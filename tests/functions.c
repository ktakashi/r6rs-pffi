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

/* TODO more */
