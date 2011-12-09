/******************************************************************************
 * description: This program reads rules for pascal grammar from an input
 *              file, saves those into an output file, constructs code to
 *              execute upon applying a rule, and inserts the constructed
 *              code on the next line following the rule.
 * author:      Martin Dluhos
 * created:     October 29, 2011
 * revised:     November 2, 2011
 *******************************************************************************/

#include <stdio.h>
#include<string.h>
#include<stdlib.h>

#define MAX_LEN 50
#define SYMBOLS_NUM 15

int main (int argc, char *argv)
{
  int count = 0;  // the number of nonterms seen in a rule
  char nonterm[MAX_LEN+1]; // current parent nonterm
  char symbol[MAX_LEN+1]; // current symbol
  char *line[SYMBOLS_NUM]; // the current line (an array of strings)
  /* This array saves the positions of nonterms in a line. */
  int positions[SYMBOLS_NUM]; 
  int index = 0; // current index in the line array
  int i;

  /* Open infile and outfile. */
  FILE *infile = fopen ("pascal_rules.y", "r");
  FILE *outfile = fopen ("pascal_out.y", "w");

  /* Read the parent nonterm, save it and print it. */
  fscanf (infile, "%s", nonterm);
  fprintf (outfile, "%s\n", nonterm);

  /* Keep processing nonterms while the next string read is a parent nonterm. */
  while (strcmp (nonterm, "%%") != 0)
  {
    /* The next string should be a colon, so do not increase count. */
    fscanf (infile, "%s", symbol);
    line[index] = malloc (strlen (symbol) * sizeof (char) + 1);
    strcpy (line[index], symbol);

    while (strcmp (line[index], ";") != 0)
    {
      /* Keep reading the rule while the current string is not a pipe or
       * semicolon. 
       */
      while ((strcmp (line[index], "|") != 0) && (strcmp (line[index], ";") != 0))
      {

        /* If the current string starts with a lowercase letter and is not
         * epsilon, then it is a nonterm, so store the position of nonterms 
         * in positions and increment nonterm count.
         */
        if (line[index][0] >= 'a' && line[index][0] <= 'z' && strcmp (line[index], "epsilon") != 0)
          positions[count++] = index;

        /* Handle the special case of epsilon. */
        if (strcmp (line[index], "epsilon") == 0)
          count = -1; // indicate that epsilon was encountered
        
        /* Read the next string. */
        fscanf (infile, "%s", symbol);
        index++;
        line[index] = malloc (strlen (symbol) * sizeof (char) + 1);
        strcpy (line[index], symbol);

      } // while term or nonterm

      /* We have reached the end of line, so append the line to outfile. */
      /* Something weird is happening here! */
      fprintf(outfile, "  ");
      for (i = 0; i < index; i++)
        fprintf(outfile, "%s ", line[i]);
      fprintf (outfile, "\n    ");

      /* Insert a new line and the relevant function or code appropriately indented. */
      switch(count)
      {
        /* Encountered epsilon- insert node with empty_statement as a child. */
        case -1: 
          fprintf (outfile, "{ $$ = simple_nonterm_unary_tree (_%s, _empty_statement); }\n", nonterm);
          break;
        case 0: // encountered a single terminal
          fprintf (outfile, "{ $$ = simple_unary_tree (_%s, %s); }\n", nonterm, line[1]);
          break;
        case 1: 
          fprintf (outfile, "{ $$ = $%d; }\n", positions[0]);
          break;
        case 2: 
          fprintf (outfile, "{ $$ = simple_binary_tree (_%s, $%d, $%d); }\n", nonterm, positions[0], positions[1]);
          break;
        default:
          fprintf (outfile, "{\n      ");
          fprintf (outfile, "AttributeSet *attributes = new_attribute_set (0);");
          fprintf (outfile, "\n      ");
          fprintf (outfile, "Node *node = new_nnode (_%s, %d, attributes);\n", nonterm, count);
          for (i = 0; i < count; i++)
            fprintf (outfile, "      set_child (node, %d, $%d);\n", i, positions[i]);
          fprintf (outfile, "      $$ = node;\n"); 
          fprintf (outfile, "    }\n");
          break;
      } // switch

      /* If the last string read was a pipe, then save the pipe and start processing
       * the next line.
       */
      if (strcmp (line[index], "|") == 0)
      {   
        index = 0; // reset both indices
        count = 0;

        line[index] = malloc (sizeof (char) + 1);
        line[index] = "|";

        /* Read the next string on a line. */
        fscanf (infile, "%s", symbol);
        index++;
        line[index] = malloc (strlen (symbol) * sizeof (char) + 1);
        strcpy (line[index], symbol);
      } 
    } // while rule

    /* We have finished processing rules for the current nonterm,
     * so terminate them properly.
     */ 
    fprintf (outfile, "  ;\n\n");

    index = 0; // reset both indices
    count = 0;

    /* Read the next parent nonterm, save it and print it. */
    fscanf (infile, "%s", nonterm);
    fprintf (outfile, "%s\n", nonterm);

  } // while nonterm

  /* Close infile and outfile. */
  fclose(infile);
  fclose(outfile);
  return 0;
} // main
