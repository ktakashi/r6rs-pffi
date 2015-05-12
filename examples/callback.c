#include <stdio.h>
#include <stdint.h>

/* From Chez Scheme Version 8 User's Guide */

typedef void (*CB)(char);

CB callbacks[256];

void cb_init(void) {
  int i;

  for (i = 0; i < 256; i += 1)
    callbacks[i] = (CB)0;
}

/* modified to intptr_t to shut compiler up */
void register_callback(char c, intptr_t cb) {
  callbacks[c] = (CB)cb;
}

void event_loop(void) {
  CB f; char c;

  for (;;) {
    c = getchar();
    if (c == EOF) break;
    f = callbacks[c];
    if (f != (CB)0) f(c);
  }
}

/* gcc -shared -fPIC -o callback.so callback.c */
