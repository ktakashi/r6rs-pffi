#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <wctype.h>

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

int callback_proc2(int (*f)(int *), int n)
{
  return f(&n);
}

void* callback_proc3(void* (*f)(int *), int n)
{
  return f(&n);
}


void* id_str(char *s)
{
  return (void *)s;
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

void fill_n(int *arr, int size, int (*f)(int))
{
  int i;
  for (i = 0; i < size; i++) {
    arr[i] = f(i + 1);
  }  
}


int sum(int n, ...)
{
  va_list l;
  int sum = 0;
  int x;
  
  va_start(l, n);

  for (x = 0; x < n; x++) {
    sum += va_arg(l, int);
  }
  
  va_end(l);
  return sum;
  
}

struct st1
{
  int count;
  int *elements;
};
struct st2
{
  struct st1 p;
  short attr;
};


void fill_st_values(struct st2 *st)
{
  int i;
  st->p.count = 10;
  st->p.elements = (int *)malloc(sizeof(int) * 10);
  /* fprintf(stderr, "%p:%u\n", st->p.elements, offsetof(struct st1, elements)); */
  for (i = 0; i < 10; i++) st->p.elements[i] = i;
  st->attr = 5;
}

void free_st_values(struct st2 *st)
{
  free(st->p.elements);
}

/* for boolean test */
int is_even(int n) 
{
  return n % 2 == 0;
}

int is_odd(int n) 
{
  return n % 2 != 0;
}

int check_dispatch(int n, int check_even) 
{
  if (check_even) {
    return is_even(n);
  } else {
    return is_odd(n);
  }
}

extern int int_array[];
int int_array[10] = {1,2,3,4,5,6,7,8,9,10};

int * get_int_array() {
  return int_array;
}

extern int * int_pointer;
static int int_value = 100;
int * int_pointer = &int_value;

int initial_int_pointer_value()
{
  return int_value;
}

wchar_t wtoupper(wchar_t wc)
{
  return towupper(wc);
}

wchar_t wcallback(wchar_t wc, wchar_t (* proc)(wchar_t))
{
  return proc(wc);
}


/* TODO more */
