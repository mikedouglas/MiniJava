#include "runtime.h"

/*
 * A sample C program that calls a function. The
 * resulting .s file is handed out in lecture to
 * have the students "reverse engineer" the stack
 * frame layout.
 */
 
int sample_fun(int x, int y) {
   int temp = x-y;
   int temp2 = x+y;
   /* mj_println(temp); debug */
   return temp*temp2;
} 
 
void mj_main() {
  mj_println(sample_fun(10, 20));
}
