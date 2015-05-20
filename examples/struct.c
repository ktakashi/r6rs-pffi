struct st1
{
  int count;
  void *elements;
};

struct st2
{
  struct st1 p;
  short attr;
};

static int values[] = {0,1,2,3,4,5,6,7,8,9};

#define CONST 1L
#define INT_ARRAY 1L<<1;

void fill_struct(struct st2 *s)
{
  s->p.count = sizeof(values)/sizeof(values[0]);
  s->p.elements = (void *)values;
  s->attr = CONST | INT_ARRAY;
}

/* gcc -fPIC -o struct.so -shared struct.c */
