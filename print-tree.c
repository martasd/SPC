/**
 * print-tree.c
 *   A simple parser that builds a parse tree and then prints it out.
 *
 * Copyright (c) 2011 Samuel A. Rebelsky, All Rights Reserved
 *
 *   This program is free software: you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public License
 *   as published by the Free Software Foundation, either version 3 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General
 *   Public License along with this program.  If not, see
 *   <http://www.gnu.org/licenses/>.
 */


// +---------+--------------------------------------------------------
// | Headers |
// +---------+

#include <stdlib.h>                     // For <stdlib.h>

#include "parse-tree.h"                 // For print_tree and free_tree

// Things we get from the Yacc
extern void yyparse (void);

// Things our parser builds
extern Node *tree;


// +------+-----------------------------------------------------------
// | Main |
// +------+

int
main (int argc, char **argv)
{
  yyparse ();
  print_tree (stdout, tree);
  printf ("tree has been printed\n");
  free_tree (tree);
  printf ("tree has been freed\n");
  exit (0);
} // main 

