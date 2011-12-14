/**
 * pi.c
 *   Our simple Pascal interpreter.  Run pi -help for information
 *   on command-line flags.
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

#include <stdio.h>      // For stdin
#include <stdlib.h>     // For atoi
#include <string.h>     // For strcmp

#include "stac.h"


// +--------------------+---------------------------------------------
// | External Variables |
// +--------------------+

extern Instruction *instructions;

extern int max_instructions;

extern int num_instructions;

extern FILE *yyin;

extern void yyparse (void);


// +---------+--------------------------------------------------------
// | Helpers |
// +---------+

/**
 * Print out the help message.
 */
void
print_help (char *progname)
{
  printf ("Usage: %s [-s size] [-m size] [-help] [-p] [-e] [-|file]\n", 
          progname);
  printf ("  -s size\n");
  printf ("    Permit programs of up to size instructions.  (Default 1024.)\n");
  printf ("  -m size\n");
  printf ("    Permit programs of up to size bytes.  (Default 1024.)\n");
  printf ("  -help, --help\n");
  printf ("    Print the help message.\n");
  printf ("  -p\n");
  printf ("    Prints the assembly program\n");
  printf ("  -n\n");
  printf ("    Do not execute the program\n");
  printf ("  -v\n");
  printf ("    Verbose execution\n");
} // print_help


// +------+-----------------------------------------------------------
// | Main |
// +------+

int
main (int argc, char **argv)
{
  int i;
  int print = 0;        // Do we print the program first?
  int execute = 1;      // Do we execute the program?
  int verbose = 0;      // Should execution be verbose
  int memsize = 1024;   // The size of memory
  int progsize = 1024;  // The size of the program
  FILE *infile = NULL;  // Where we get the program from

  // Parse parameters
  for (i = 1; i < argc; i++)
    {
      if ((! strcmp (argv[i], "-help")) || (! strcmp (argv[i], "--help")))
        {
          print_help (argv[0]);
        }

      else if (! strcmp (argv[i], "-"))
        {
          if (infile != NULL)
            {
              fprintf (stderr, "Cannot specify both a file and stdin.\n");
              return -1;
            }
          else
            infile = stdin;
        }

      else if (! strcmp (argv[i], "-n"))
        {
          execute = 0;
        }

      else if (! strcmp (argv[i], "-p"))
        {
          print = 1;
        }

      else if (! strcmp (argv[i], "-s"))
        {
          progsize = atoi (argv[++i]);
        }

      else if (! strcmp (argv[i], "-v"))
        {
          execute = 1;
          verbose = 1;
        }

      else 
        {
          if (infile != NULL)
            {
              fprintf (stderr, "Cannot open multiple files.\n");
              return -1;
            }
          infile = fopen (argv[i], "r");
          if (infile == NULL)
            {
              fprintf (stderr, "Cannot open file %s.\n", argv[i]);
              return -1;
            }
        } // default
    } // for

  stac_init ();
  max_instructions = progsize;
  instructions = calloc (progsize, sizeof (Instruction));

  // Read the input
  yyin = infile;
  yyparse ();

  // Print the program
  if (print)
    print_program (stdout, num_instructions, instructions, 1);

  // Run the program
  if (execute)
    return run_program (num_instructions, instructions, memsize, verbose);

  // Clean up
  free (instructions);

  // And we're done
  return 0;
} // main
