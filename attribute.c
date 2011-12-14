/**
 * attribute.c
 *   Structures to help us track attributes (most typically, in parse trees)
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
  1. This is primarily intended to be used with Yacc/Bison grammars. 
     However, I hope that it can also stand on its own.  Hence, I've 
     tried to make it detect whether or not it has been included in
     a .y file by checking if YYBISON is defined.
 */


// +---------+--------------------------------------------------------
// | Headers |
// +---------+

#ifndef YYBISON
#include "attribute.h"
#endif


// +---------+--------------------------------------------------------
// | Helpers |
// +---------+

/**
 * Find the index of an attribute in a set.  Returns -1 if not found.
 */
int
find_attribute (AttributeSet *set, char *name)
{
  int i;
  for (i = 0; i < set->size; i++)
    {
      if (! strcmp (set->contents[i].name, name))
        {
          return i;
        } // if found
    } // for
  // If we've gotten this far, it's not there
  return -1;
} // find_attribute

/**
 * Set an attribute.
 */
int
set_attribute (AttributeSet *set, char *name, Attribute att)
{
  // See if the attribute is already there.
  int index = find_attribute (set, name);
  // If so, update the value.
  if (index >= 0)
    {
      set->contents[index].val = att;
      return 1;
    } // if

  // If there's not room for another atttribute, give up
  if (set->size >= set->capacity)
    return 0;

  // Allocate a new attribute
  index = (set->size)++;
  set->contents[index].name = name;
  set->contents[index].val = att;
  return 1;
} // set_i_attribute

// +--------------------+---------------------------------------------
// | Exported Functions |
// +--------------------+

void
free_attribute_set (AttributeSet *set)
{
  /* Failing here when trying to free variable_declaration attributes. */
  free (set->contents);
  set->contents = NULL;
  set->size = 0;
  set->capacity = 0;
  free (set);
} // free_attribute_set

int
get_i_attribute (AttributeSet *set, char *name)
{
  return set->contents[find_attribute (set, name)].val.ival;
} // get_i_attribute

char
get_c_attribute (AttributeSet *set, char *name)
{
  return set->contents[find_attribute (set, name)].val.cval;
} // get_c_attribute

int
get_d_attribute (AttributeSet *set, char *name)
{
  return set->contents[find_attribute (set, name)].val.dval;
} // get_d_attribute

void *
get_p_attribute (AttributeSet *set, char *name)
{
  return set->contents[find_attribute (set, name)].val.pval;
} // get_p_attribute

double
get_r_attribute (AttributeSet *set, char *name)
{
  return set->contents[find_attribute (set, name)].val.dval;
} // get_r_attribute

char *
get_s_attribute (AttributeSet *set, char *name)
{
  return set->contents[find_attribute (set, name)].val.sval;
} // get_s_attribute

int
has_attribute (AttributeSet *set, char *name)
{
  return (find_attribute (set, name) != -1);
} // has_attribute

AttributeSet *
new_attribute_set (int capacity)
{
  int i;
  AttributeSet *result = malloc (sizeof (AttributeSet));
  if (result == NULL)
    return NULL;
  if (capacity > 0)
    {
      result->contents = malloc (capacity * sizeof (NamedAttribute));
      if (result->contents == NULL)
        {
          free (result);
          return NULL;
        }
      for (i = 0; i < capacity; i++)
        result->contents[i].name = "";
    } // if capacity > 0
  result->capacity = capacity;
  result->size = 0;
  return result;
} // new_attribute_set

int 
set_c_attribute (AttributeSet *set, char *name, char cval)
{
  Attribute att;
  att.cval = cval;
  return set_attribute (set, name, att);
} // set_c_attribute

int 
set_d_attribute (AttributeSet *set, char *name, double dval)
{
  Attribute att;
  att.dval = dval;
  return set_attribute (set, name, att);
} // set_d_attribute

int
set_i_attribute (AttributeSet *set, char *name, int ival)
{
  // It would probably work to cast ival as an Attribute, but
  // this is safer.
  Attribute att;
  att.ival = ival;
  return set_attribute (set, name, att);
} // set_i_attribute

int
set_p_attribute (AttributeSet *set, char *name, void *pval)
{
  Attribute att;
  att.pval = pval;
  return set_attribute (set, name, att);
} // set_p_attribute

int
set_r_attribute (AttributeSet *set, char *name, float fval)
{
  Attribute att;
  att.fval = fval;
  return set_attribute (set, name, att);
} // set_r_attribute

int
set_s_attribute (AttributeSet *set, char *name, char *sval)
{
  Attribute att;
  att.sval = sval;
  return set_attribute (set, name, att);
} // set_s_attribute

