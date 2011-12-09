/**
 * check-mem.c
 *   A sanity check on our parser.  Makes sure that if you create a
 *   parse tree and then free it, you've not left any used memory
 *   claimed.
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

#include <malloc.h>             // For malloc_stats
#include <stdlib.h>             // For exit
#include "parse-tree.h"		// For free_tree

// Things we get from the Yacc
extern void yyparse (void);

// Things our parser builds
extern Node *tree;


// +---------+--------------------------------------------------------
// | Helpers |
// +---------+

/**
 * Determine how much memory has been allocated by malloc.
 */
static int
mem_allocated ()
{
  struct mallinfo info = mallinfo ();
  return info.uordblks;
} // mem_allocated


// +------+-----------------------------------------------------------
// | Main |
// +------+

int
main (int argc, char **argv)
{
  int allocated_start = mem_allocated ();
  yyparse ();
  // free_tree (tree);
  int allocated_end = mem_allocated ();
  if (allocated_start != allocated_end)
    {
      fprintf (stderr, "Memory leak %d blocks\n", 
               allocated_end - allocated_start);
      exit (1);
    }
  exit (0);
} // main 
