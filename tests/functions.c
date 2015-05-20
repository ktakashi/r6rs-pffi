#include <stdlib.h>
#include <stdio.h>

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
  /* fprintf(stderr, "%p:%lu\n", st->p.elements, offsetof(struct st1, elements)); */
  for (i = 0; i < 10; i++) st->p.elements[i] = i;
  st->attr = 5;
}

void free_st_values(struct st2 *st)
{
  free(st->p.elements);
}


/* TODO more */
