# A simple Makefile for building a simple parser for Pascal.
# author:  Sam Rebelsky (modified by Martin Dluhos)
# revised: December 11, 2011

CFLAGS=-g -Wall `pkg-config --cflags glib-2.0`
LDFLAGS= `pkg-config --libs glib-2.0`
YACC=bison
LEX=flex
LFLAGS=-i
INCLUDED_FILES = parse-tree.h \
                 parse-tree.c \
                 attribute.h \
                 attribute.c

# The various .o files that are needed for executables.
OBJECT_FILES = pascal.tab.o symtab.o staclib.o

default: pi

pi: pi.o $(OBJECT_FILES)
		$(LINK.o) -o $@ $^

print-tree: print-tree.o $(OBJECT_FILES)
	$(LINK.o) -o $@ $^

check-mem: check-mem.o $(OBJECT_FILES)
	$(LINK.o) -o $@ $^

pascal.tab.o: pascal.tab.c lex.yy.c $(INCLUDED_FILES)

print-tree.o: parse-tree.h 

pascal.tab.c: pascal.y
	$(YACC) $(YFLAGS) $^

lex.yy.c: pascal.l
	$(LEX) $(LFLAGS) $^

