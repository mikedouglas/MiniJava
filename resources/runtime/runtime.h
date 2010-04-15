/*
 * A minuscule "runtime" environment for minijava.
 *
 * We develop this in C and compile it with the
 * gcc compiler.
 *
 * See the accompanying makefile.
 */


/* 
 * This function should be created by the minijava compiler. 
 * It will be called by the runtime to start the minijava program.
 */
void mj_main();

/*
 * The mj runtime defines a function to print an integer.
 */
void mj_println(int i);

/*
 * Allocate memory for an object and intialize it.
 */
char* mj_new_object(int numbytes);

/*
 * Allocate memory for an array and intialize it.
 * 
 * The returned pointer points to element 0 of the
 * array. The element at index[-1] position is the 
 * length of the array.
 */
int* mj_new_array(int numElements);
