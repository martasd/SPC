#ifndef __PARSE_TREE_H__
#define __PARSE_TREE_H__

/**
 * parse-tree.h
 *   A simple structure for parse trees.
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
  1. We represent nodes using the traditional "objects in C" strategy
     a. Every "object" begins with an integer that represents its type.
        (Traditionally, these are randomly generated.  In this case, we
        have so few objects that we just use some predefined numbers.)
        Note: This strategy usually needs to be slightly more sophisticated
        to handle type checking, but I'm lazy.
     b. Inheritance is supported through structure inclusion.  Since 
        the C standard says that the elements of a struct must be laid
        out in order, by making an element of the superclass the first
        element, we ensure that its fields are contained within.
 */


// +---------+--------------------------------------------------------
// | Headers |
// +---------+

#include <stdio.h>

#include "attribute.h"


// +-------+----------------------------------------------------------
// | Types |
// +-------+

/**
 * Generic nodes simply have a type (for objects), a symbol (terminal
 * or nonterminal), and a set of attributes.
 */        
extern int TYPE_NODE;
typedef struct Node
  {
    int type;
    int symbol;
    AttributeSet *attributes;
  } Node;

/**
 * Nodes containing terminals/tokens have no additional information.
 * However, we provide a separate type for clarity.
 */
extern int TYPE_TNODE;
typedef struct TNode
  {
    Node parent;
  } TNode;

/**
 * Nodes containing nonterminals may have a set of children.
 */
extern int TYPE_NNODE;
typedef struct NNode
  {
    Node parent;
    int arity;          // The number of children
    Node **children;    // The subtrees
  } NNode;


// +-----------+------------------------------------------------------
// | Functions |
// +-----------+

/**
 * Free the memory allocated to a parse tree.
 */
void free_tree (Node *tree);

/*
 * Get the ith subtree of a tree.
 */
Node *get_child (Node *tree, int i);

/**
 * Determine if a Node is an NNode.
 */
int is_nnode (Node *tree);

/**
 * Determine if a Node is a TNode.
 */
int is_tnode (Node *tree);

/**
 * Create a new nonterminal node that holds the given information.  Returns
 * the node if successful.  Returns NULL if not successful.
 */
Node *new_nnode (int nonterm, int arity, AttributeSet *attributes);

/**
 * Create a new terminal node that holds the given information.  Returns
 * the node if successful.  Returns NULL if not successful.
 */
Node *new_tnode (int term, AttributeSet *attributes);

/** 
 * Print a tree to stream.
 */
void print_tree (FILE *stream, Node *node);

/**
 * Set the ith child of a tree.  Returns 1 upon success and 0 upon
 * failure.
 */
int set_child (Node *node, int i, Node *child);

#endif // __PARSE_TREE_H__
