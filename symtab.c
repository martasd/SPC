/******************************************************************************
 * description: Defines functions for managing the symbol table for a simple 
 *              pascal compiler.
 * author:      Martin Dluhos
 * created:     November 6, 2011
 * revised:     November 24, 2011
 *****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "symtab.h"
#include "attribute.h"

/* Allocate space for and initialize a new symbol table. */
SymTab *
symtab_new ()
{
  int i;
  /* Allocate space for the symbol table. */
  SymTab *stab = malloc (sizeof (SymTab));
  if (stab == NULL)
    return NULL;

  stab->scopes = malloc (SCOPES_NUM * sizeof (Scope));
  if (stab->scopes == NULL)
  {
    free (stab);
    return NULL;
  }

  /* Create the outermost scope. */
  stab->scopes[0].scope = g_hash_table_new_full (g_str_hash, g_str_equal, (GDestroyNotify) free, (GDestroyNotify) free_attribute_set);
  /* Initialize the rest of the empty table with NULL. */
  for (i = 1; i < SCOPES_NUM; i++)
    stab->scopes[i].scope = NULL; 

  stab->capacity = SCOPES_NUM;
  stab->size = 1; // the top-level scope is the first scope so size is 1

  return stab;
} // symtab_new


/* Free the space allocated to a symbol table.
 * NOTE: Even after destroying the table a constant amount of memory
 * remains unfreed, which is due to the implementation of GHashTable. This
 * amount of unfreed memory is so small that this memory leak is not an
 * significant concern.
 */
void
symtab_free (SymTab *stab)
{
  int i;
  /* Free the space allocated for keys and values in the hash table and 
   * subsequently deallocate space for individual hash tables. */
  for (i = 0; i < stab->size; i++)
    g_hash_table_destroy (stab->scopes[i].scope);

  free (stab->scopes); 

  stab->scopes = NULL;
  stab->size = 0;
  stab->capacity = 0;
  free (stab);
} // symtab_free


/* Enter a new scope. 
 * Returns true (1) if it succeeds and false (0) if it fails.
 */
int
symtab_enter (SymTab *stab)
{
  int i;
  /* If the array is full, double the capacity. */
  if (stab->size == stab->capacity)
  {
    /* Reallocate the space for the scopes array, so that capacity doubles. */
    Scope *temp = realloc (stab->scopes, 2 * stab->capacity * sizeof (Scope)); 
    if (temp == NULL)
      return 0;
    stab->scopes = temp;
    stab->capacity = 2 * stab->capacity; 

    /* Initialize the second half of the table with NULL. */
    for (i = stab->size; i < stab->capacity; i++)
      stab->scopes[i].scope = NULL; //g_hash_table_new (g_str_hash, g_str_equal);

  } // if (stab->size == stab->capacity)

  /* Create a new scope. */
  stab->scopes[stab->size].scope = g_hash_table_new_full (g_str_hash, g_str_equal, (GDestroyNotify) free, (GDestroyNotify) free_attribute_set);
  stab->size++;

  return 1;
} // symtab_enter


/* Exit the scope most recently entered.*/
int
symtab_exit (SymTab *stab)
{

  /* If we are in the topmost scope, do not update current scope and 
   * indicate failure. 
   */
  if (stab->size == 1)
    return 0;
  
  /* Destroy the hash we are exiting. */
  g_hash_table_destroy (stab->scopes[stab->size-1].scope);
  
  /* Return to the parent scope. */
  stab->size--;
  return 1;
} // symtab_exit


/* Add a symbol and its attributes to the table. Makes a copy of sym. 
 * Returns true (1) if it succeeds and false (0) otherwise.
 */
int
symtab_put (SymTab *stab, char *sym, AttributeSet *attributes)
{
  /* Make a copy of sym. */
  char *symbol = g_strdup (sym);

  /* If the symbol has been declared in the current scope,
   * then indicate failure. Otherwise, put the (symbol, attributes) pair in.
   */
  if (symtab_is_in_scope (stab, symbol) == 1)
    return 0;
  else
  {
    g_hash_table_insert(stab->scopes[stab->size-1].scope, symbol, attributes);
    return 1; 
  }
} // symtab_put


/* Update the value of one attribute of a symbol. 
 * That attribute must already be associated with the symbol. 
 * Returns true (1) if it succeeds and false (0) otherwise.
 */
int
symtab_update (SymTab *stab, char *sym, char *attname, Attribute attval)
{
  int index;

  /* If the symbol is in the table, then update its value. */
  if ((index = symtab_is_declared (stab, sym)) != -1)
  {
    /* Get the attributes of the symbol. */
    AttributeSet *attributes = 
      g_hash_table_lookup (stab->scopes[index].scope, sym);
    if (attributes == NULL)
      fprintf(stderr, "Symbol %s does not have attributes.\n", sym);

    /* If the attribute does not exist, then indicate failure.
     * Otherwise, update the value of the attribute and put the
     * attributes back as the value of the symbol.
     */
    if (find_attribute (attributes, attname) == -1)
      return 0;
    else
    {
      set_attribute (attributes, attname, attval);
      return 1;
    } // if find_attribute
  }
  /* Otherwise, the symbol has not been declared, so indicate failure. */
  else 
    return 0;
} // symtab_update


/* Get the attributes associated with symbol, using normal scoping rules. 
 * Returns NULL if the symbol is not in the table.
 */
AttributeSet *
symtab_get (SymTab *stab, char *sym)
{
  int index;
  /* If the symbol is not in the table, return NULL.
   * Otherwise, return the attributes associated with the symbol.
   */
  if ((index = symtab_is_declared (stab, sym)) == -1)
    return NULL;
  else
    return g_hash_table_lookup (stab->scopes[index].scope, sym);
} // symtab_get


/* Determine if the symbol has been declared in the current scope.
 * Returns 1 on success.
 */
int
symtab_is_in_scope (SymTab *stab, char *sym)
{
  AttributeSet *attributes = 
    g_hash_table_lookup (stab->scopes[stab->size-1].scope, sym);

  /* If the symbol has been found, indicate success. */
  if (attributes != NULL)
    return 1;
  else 
    return 0;
} // symtab_is_in_scope


/* Determine if the symbol has been declared at some point - in the current 
 * scope, in an enclosing scope, at the top level, etc..
 * Returns the index of the topmost scope where the symbol is declared. If the
 * symbol has not been declared, returns -1.
 */
int
symtab_is_declared (SymTab *stab, char *sym)
{
  /* Save the original size, so that it can be restored later. */
  int sym_scope;
  int orig_size = stab->size;

  /* Search through the predecessor scopes. */
  while (stab->size > 0)
  {
    if (symtab_is_in_scope (stab, sym) == 1)
    {
      sym_scope = stab->size - 1;
      stab->size = orig_size;
      return sym_scope;
    }
    stab->size--;
  }

  /* Restore the original size. */
  stab->size = orig_size;

  /* The symbol has not been found in any scope, so it has not been declared. */
  return -1;
} // symtab_is_declared
