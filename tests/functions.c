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

void fill_one(int *arr, int size)
{
  int i;
  for (i = 0; i < size; i++) {
    arr[i] = 1;
  }  
}

/* TODO more */
