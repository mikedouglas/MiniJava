#include "runtime.h"

#include <stdio.h>
#include <stdlib.h>

/*
 * The minijava runtime defines a main function which calls the minijava main
 * function.
 */
int main() {
  mj_main();
  return 0;
}

/*
 * The mj runtime defines a function to print an integer.
 */
void mj_println(int i) {
  printf("%d\n", i);
}

/*
 * Allocate memory for an object and initialize it.
 */
char* mj_new_object(int numbytes) {
  char* object = malloc(numbytes);
  int i;
  for (i=0; i<numbytes; i++)
    object[i] = 0;
  return object;
}

/*
 * Allocate memory for an array and initialize it.
 * 
 * The returned pointer points to element 0 of the
 * array. The element at index[-1] position is the 
 * length of the array.
 */
int* mj_new_array(int numElements) {
  int* arr = malloc((numElements+1)*sizeof(int));
  int i;
  arr[0] = numElements;
  arr++;
  for (i=0; i<numElements; i++) {
    arr[i] = 0;
  }
  return arr;
}
