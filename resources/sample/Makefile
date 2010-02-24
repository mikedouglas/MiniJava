#
# This is a GNU make file to generate the "reference" output
# by compiling and running the MiniJava sample code in this
# directory with the JDK.
#
# Usage: 
#   Assuming that you have gnu make and a standard JDK installed
#   on your system, simply type "make" from a shell prompt in
#   the directory where you found this make file.
#
# You can add your own .java files in this directory and
# run this make file to produce their expected output.
# The minijava JUnit tests will then also compile and 
# run these additional tests.
#
# Author: Kris De Volder
#

JAVAFILES= $(wildcard *.java)
OUTFILES=  $(patsubst %.java, %.out, $(JAVAFILES))

goal:	$(OUTFILES)

clean:
	rm -f *.class

%.out:	%.class
	java $(basename $<) > $@
	make clean

%.class: %.java
	javac $<
