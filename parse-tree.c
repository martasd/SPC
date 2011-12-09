/**
 * parse-tree.c
 *   Support procedures for a simple parse-tree structure.
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

// +-------+----------------------------------------------------------
// | Notes |
// +-------+

/*
  1. This code depends upon values defined by Yacc/Bison.  Hence, it 
     should only be included within a .y file, not compiled on its
     own.

  2. This code depends upon functions defined in attribute.h and
     attribute.c.  It should be linked with those files.

  3. Because of these and other dependencies, the .y file should also
     include <stdio.h>, "parse-tree.h", and "attribute.h".
 */


// +--------------------+---------------------------------------------
// | Exported Variables |
// +--------------------+

int TYPE_NODE = 61764;
int TYPE_TNODE = 73195;
int TYPE_NNODE = 80698;


// +-----------------+------------------------------------------------
// | Predeclarations |
// +-----------------+

static void print_tree_indented (FILE *stream, Node *node, int spaces);


// +---------+--------------------------------------------------------
// | Helpers |
// +---------+

/**
 * Print a compound node, indented by some number of spaces.
 * As in print_tree_indented, we assume that the first indent
 * has already been printed.
 */
static void
print_nnode (FILE *stream, NNode *nn, int spaces)
{
  Node *node = (Node *) nn;
  // Sanity check
  if (node == NULL)
    {
      fprintf (stream, "*** ERROR: Null NNode ***\n");
      return;
    }
  // Print the current node
  fprintf (stream, "%s/%d\n", nonterm_names[node->symbol], nn->arity);
  // printf ("%d\n", nn->arity);
  // Print all of the children
  int child_indent = spaces + 4;
  int i;
  for (i = 0; i < nn->arity; i++)
    {
      fprintf (stream, "%*d ", child_indent, i+1);
      print_tree_indented (stream, nn->children[i], child_indent);
    } // for
} // print_nnode

/**
 * Print a terminal node.
 */
static void
print_tnode (FILE *stream, TNode *tn)
{
  int symbol = ((Node *) tn)->symbol;
  int index = symbol - TOKENS_START;
  AttributeSet *attributes = ((Node *) tn)->attributes;

  switch (symbol)
    {
      case _INTEGER:
        fprintf (stream, "INT[%d]\n", 
                 get_i_attribute (attributes, "ivalue"));
        break;

      case _REAL:
        fprintf (stream, "REAL[%lf]\n", 
                 get_r_attribute (attributes, "rvalue"));
        break;

      case _IDENTIFIER:
        fprintf (stream, "ID[%s]\n", 
                 get_s_attribute (attributes, "name"));
        break;

      case _STRING:
        fprintf (stream, "STRING[%s]\n", 
                 get_s_attribute (attributes, "svalue"));
        break;

      default:
        if (has_attribute (attributes, "text"))
          fprintf (stream, "%s\n", get_s_attribute (attributes, "text"));
        else if ((symbol < TOKENS_START) || (symbol > TOKENS_END))
          fprintf (stream, "*invalid*[%d]\n", symbol);
        else
          fprintf (stream, "%s\n", toknames[index]);
        break;
    } // switch
} // print_tnode

/**
 * Print a tree, indented by some number of spaces.
 * We assume that the indent has already been printed for the first
 * line.  (Yeah, that's strange, but it makes some thing easier.)
 */
static void
print_tree_indented (FILE *stream, Node *node, int spaces)
{
  if (node == NULL)
    {
      fprintf (stream, "*** ERROR: Null tree. ***\n");
    }
  else if (is_tnode (node))
    {
      print_tnode (stream, (TNode *) node);
    } // if it's a leaf node
  else if (is_nnode (node))
    {
      print_nnode (stream, (NNode *) node, spaces);
    } // if it's a compound node.
  else
    {
      fprintf (stream, "*** ERROR: Unknown node type %d. ***\n", node->type);
    }
} // print_tree_indented


// +--------------------+---------------------------------------------
// | Exported Functions |
// +--------------------+

void
free_tree (Node *node)
{
  if (! node)
    return;
  if (is_nnode (node))
    {
      int i;
      NNode *nn  = ((NNode *) node);
      for (i = 0; i < nn->arity; i++)
      {
   //     if (nn->children[i]->type == TYPE_TNODE)
   //       printf ("type: TERM\n");
   //     else
   //       printf ("type: NONTERM\n");
   //     //printf ("symbol: %s\n", nonterm_names[nn->children[i]->symbol]);
        free_tree (nn->children[i]);
      }
      if (nn->children != NULL)
        free (nn->children);
    } // if it's a nonterminal node
  //  if (node->type == TYPE_NNODE)
  //    printf ("symbol 1: %s\n", nonterm_names[node->symbol]);
  //  else
  //  {
  //    printf ("symbol: %s\n", toknames[node->symbol]);
  //    printf ("terminal! %s\n", get_s_attribute (node->attributes, "name"));
  //  }
  // 
  //if (node->type == TYPE_NNODE)
  //  printf ("symbol 2: %s\n", nonterm_names[node->symbol]);
  free_attribute_set (node->attributes);
  free (node);
} // free_tree

Node *
get_child (Node *node, int i)
{
  if (! is_nnode (node))
    return NULL;
  NNode *nn = (NNode *) node;
  if (nn->arity <= i)
    return NULL;
  return nn->children[i];
} // get_child

int
is_nnode (Node *node)
{
  return node->type == TYPE_NNODE;
} // is_nnode

int
is_tnode (Node *node)
{
  return node->type == TYPE_TNODE;
} // is_tnode

Node *
new_nnode (int nonterm, int arity, AttributeSet *attributes)
{
  NNode *nn = malloc (sizeof (NNode));
  if (! nn)
    return NULL;
  Node *node = (Node *) nn;
  node->type = TYPE_NNODE;
  node->symbol = nonterm;
  node->attributes = attributes;
  nn->arity = arity;
  nn->children = malloc (arity * sizeof (Node *));
  return node;
} // new_nnode

Node *
new_tnode (int term, AttributeSet *attributes)
{
  Node *tn = malloc (sizeof (TNode));
  if (! tn)
    return tn;
  Node *node = (Node *) tn;
  node->type = TYPE_TNODE;
  node->symbol = term;
  node->attributes = attributes;
  return node;
} // new_tnode

void
print_tree (FILE *stream, Node *node)
{
  print_tree_indented (stream, tree, 0);
} // print_tree

int
set_child (Node *node, int i, Node *child)
{
  // fprintf (stderr, "set_child (%p, %d, %p)\n", node, i, child);
  if (! is_nnode (node))
    return 0;
  NNode *nn = (NNode *) node;
  if (nn->arity <= i)
    return 0;
  nn->children[i] = child;
  return 1;
} // set_child
