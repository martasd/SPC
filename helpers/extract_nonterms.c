#include <stdio.h>

#define MAX_LENGTH 100

int main (int argv, char* args)
{
  FILE *infile, *outfile;
  char line[MAX_LENGTH];

  /* Open the file. */
  infile = fopen ("rules.txt", "r");
  if (infile == NULL)
  {
    printf("rules.txt cannot be opened for reading!");
    return 1;
  }

  /* Open the output file for writing */
  outfile = fopen ("nonterms.txt", "w");
  if (outfile == NULL)
  {
    printf("nonterms.txt cannot be opened for writing!");
    return 1;
  }

  /* Read the next line until end of file. */
  while (fgets (line, MAX_LENGTH, infile) != NULL)
  {
    /* If the first character of the line is not a space, then the line
     * contains a nonterm, so append it to outfile
     */
    if (line[0] != ' ' && line[0] != '\n')
      fprintf(outfile, "%s", line);
  }

  /* Close both files */
  fclose(infile);
  fclose(outfile);

  return 0;
}
