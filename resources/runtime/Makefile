#
# This is a GNU Makefile, to build the minijava runtime library.
#

# The project drop comes with an already built runtime.o file, but
# you may need to rebuild it if you are experiencing problems because
# of incompatibilities with you OS, gcc version or C library version.

# Besides a build target for building runtime.o, some other build
# targets, mainly used during testing of the runtime are defined
# in this file. 
# You will not need to use these, because the project drop contains
# Java code to call on the gnu assembler and gcc linker.

# Once you have the runtime built, you should not need to rely
# on this build script for anything anymore.

#GCC=	gcc -Wall
GCC=	gcc -Wall -pedantic
AS=	as --gstabs

########################################################################
# make runtime.o:
#      A miniscule MiniJava runtime environment in a single object file 
#      called "runtime.o"
#     
#      It is compiled from runtime.c with gcc
#   
#      See runtime.h for explanations about the "features" provided by
#      the runtime.

runtime.o: runtime.c
	$(GCC) -c $<

########################################################################
# make <something>.exe:
#
#     type "make <something>.exe" to create an executable file by
#     linking <something>.o with <runtime>.o using the gcc compiler's 
#     linker.

%.exe:	%.o
	$(GCC) -o $@ $< runtime.o

########################################################################
# make <something>.s:
#
#     To help debug the runtime library (without having to first 
#     fully implement your own mini java compiler :-)
#
#     Compile a file <something>.c into <something>.s using gcc -S.
#
#     Note: it is possible to instruct gcc (and the assembler) to use 
#     intel syntax, which is more palatable to many people than the AT&T 
#     syntax the GNU tools use by default.

%.s:	%.c
	$(GCC) -S $<
#	$(GCC) -S -masm=intel $<

########################################################################
# make <something>.o:
#
#    Build <something>.o from <something>.s
#    Note: it is possible to instruct as to use intel syntax with a
#    directive in the .s file. See also above (generate a "sample.s" file
#    with the above build target using the -masm=intel syntax.

%.o:	%.s
	$(AS) -o $@ $<

########################################################################
# type "make <something>.run to run (and first build) <something>.exe
#
#   Note that gnu make will automatically rebuild all the dependencies
#   such as <somthing>.exe runtime.o, <something>.o and <something>.s as 
#   needed.
%.run:	%.exe
	./$<

########################################################################
# type "make <something>.debug" to run <something>.exe inside of
# gdb (the gnu command line debugger).

%.debug:	%.exe
	gdb ./$<

########################################################################	
# make clean
#
#     Clean up generated object and exe files, and emacs temporary save
#     files.

clean:	
	rm -f *.o
	rm -f *.exe
	rm -f *\~

