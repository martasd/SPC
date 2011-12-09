/******************************************************************************
 * description: Functions for managing the symbol table for pascal compiler.
 * author:      Martin Dluhos
 * created:     November 6, 2011
 * revised:     November 24, 2011
 *****************************************************************************/

#ifndef __SYMTAB_H__
#define __SYMTAB_H__

#include <glib.h>
#include "attribute.h"

#define SCOPES_NUM 3

/*********
 * Types *
 *********/

/* Each scope contains a hash table and a pointer to its parent scope */
typedef struct Scope
{
  GHashTable *scope; // the hash table holds (symbol, attributes) pairs
} Scope;

/* Symbol table will be a stack of hash tables with each scope represented by
 * a separate Hash table. The stack is implemented as a dynamic array.
 */
typedef struct SymTab
{
  int capacity;
  int size; // the number of scopes created so far
  Scope *scopes; // a dynamic array of scopes
} SymTab;


/* Allocate space for and initialize a new symbol table. */
SymTab *symtab_new ();

/* Free the space allocated to a symbol table.*/
void symtab_free (SymTab *stab);

/* Enter a new scope. 
 * Returns true (1) if it succeeds and false (0) if it fails.
 */
int symtab_enter (SymTab *stab);

/* Exit the scope most recently entered.*/
int symtab_exit (SymTab *stab);

/* Add a symbol and its attributes to the table. Makes a copy of sym. 
 * Returns true (1) if it succeeds and false (0) otherwise.
 */
int symtab_put (SymTab *stab, char *sym, AttributeSet *attributes);

/* Update the value of one attribute of a symbol. 
 * That attribute must already be associated with the symbol. 
 * Returns true (1) if it succeeds and false (0) otherwise.
 */
int symtab_update (SymTab *stab, char *sym, char *attname, Attribute attval);

/* Get the attributes associated with symbol, using normal scoping rules. 
 * Returns NULL if the symbol is not in the table.
 */
AttributeSet *symtab_get (SymTab *stab, char *sym);

/* Determine if the symbol has been declared in the current scope.*/
int symtab_is_in_scope (SymTab *stab, char *sym);

/* Determine if the symbol has been declared at some point - in the current 
 * scope, in an enclosing scope, at the top level, etc..
 */
int symtab_is_declared (SymTab *stab, char *sym);

#endif // __SYMTAB_H__
