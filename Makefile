# A simple Makefile for building a simple parser for Pascal.

CFLAGS=-g -Wall `pkg-config --cflags glib-2.0`
LDFLAGS= `pkg-config --libs glib-2.0`
YACC=bison
LEX=flex
LFLAGS=-i
INCLUDED_FILES = parse-tree.h \
                 parse-tree.c \
                 attribute.h \
                 attribute.c

experiments: print-tree check-mem

print-tree: pascal.tab.o print-tree.o symtab.o
	$(LINK.o) -o $@ $^

check-mem: pascal.tab.o check-mem.o symtab.o
	$(LINK.o) -o $@ $^

pascal.tab.o: pascal.tab.c lex.yy.c $(INCLUDED_FILES)

print-tree.o: parse-tree.h 

pascal.tab.c: pascal.y
	$(YACC) $(YFLAGS) $^

lex.yy.c: pascal.l
	$(LEX) $(LFLAGS) $^

