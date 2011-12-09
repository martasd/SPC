#ifndef __ATTRIBUTE_H__
#define __ATTRIBUTE_H__

/**
 * attribute.h
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
// | Types |
// +-------+

/**
 * Names of the valid attribute types.
 */
typedef enum atttype 
  { 
    ACHAR, 
    AINT, 
    ALONG, 
    ASTRING, 
    AFLOAT, 
    ADOUBLE, 
    APOINTER,
    AMISC
  } atttype;

/**
 * A simple attribute associated with a node in a parse tree.  It is up
 * the person storing the attribute to keep track of the type of the
 * attribute.  (Alternately, you can use a TypedAttribute.)
 */
typedef union Attribute
  {
    char cval;
    int ival;
    long lval;
    char *sval;
    float fval;
    double dval;
    void *pval; // Type* can be one of these
  } Attribute;


/* The different kinds of types. */
enum TypeID
{
      // Simple types
      TYPE_BOOLEAN,
      TYPE_INTEGER,
      TYPE_REAL,
      TYPE_CHAR,
      TYPE_STRING,
      // Compound types
      TYPE_ARRAY,
      TYPE_RECORD,
      TYPE_SUBRANGE,
      TYPE_FUNCTION,
      TYPE_PROCEDURE
};
        
typedef enum TypeID TypeID;
typedef struct Type Type;
typedef struct Param Param;
/* Compound types. */
typedef struct ArrayType ArrayType;
typedef struct FunctionType FunctionType;
typedef struct ProcedureType ProcedureType;
typedef struct RecordType RecordType;
typedef struct ScalarType ScalarType;
typedef struct SubrangeType SubrangeType;

/* Store the name and type of a parameter. */
struct Param
{
  char *name;
  Type *type;
};

struct ArrayType
{
  Type *component_type;
  int dimensions;
  Type **index_types;
};

/* We need to store parameters and return value with a function. */
struct FunctionType
{
  Param **params;
  Type *return_val;
};

/* We need to store parameters with a procedure. */
struct ProcedureType
{
  Param **params;
};

struct RecordType
{
  Type *fields;
};

struct ScalarType 
{
};

/* Stores the lower and upper bound of the subrange and their type. */
struct SubrangeType
{
  Type *type;
  Attribute lower;
  Attribute upper;
};


struct Type
{
  TypeID type;
  union
  {
    ArrayType *array;
    FunctionType *function;
    ProcedureType *procedure;
    RecordType *record;
    ScalarType *scalar;
    SubrangeType *subrange;
    char *name;
    Type *base_type;        // For set, pointer,
  } info;
};

/**
 * A typed version of the aforementioned attributes.
 */
typedef struct TypedAttribute
  {
    atttype type;
    Attribute val;
  } TypedAttribute;

/**
 * A named version of the aforementioned attributes.  Typically used
 * for sets of attributes.  We assume that a name implies a type, so
 * named attributes do not have explicit types.
 */
typedef struct NamedAttribute
  {
    char *name;
    Attribute val;
  } NamedAttribute;

typedef struct AttributeSet
{
  int capacity;
  int size;			// How many attributes are currently there
  NamedAttribute *contents;	// Array of attributes associated with an id
} AttributeSet;

// +-----------+------------------------------------------------------
// | Functions |
// +-----------+

int find_attribute (AttributeSet *set, char *name);

/**
 * Create a new set of attributes that holds up to capacity attributes.
 *
 * Returns NULL upon failure.
 */
AttributeSet *new_attribute_set (int capacity);

/**
 * Free a set of attributes
 */
void free_attribute_set (AttributeSet *set);

/**
 * Set an attribute (integer, long, etc.).  Returns 1 for success and 0
 * for failure.
 */
int set_c_attribute (AttributeSet *set, char *name, char cval);
int set_d_attribute (AttributeSet *set, char *name, double dval);
int set_i_attribute (AttributeSet *set, char *name, int ival);
int set_p_attribute (AttributeSet *set, char *name, void *pval);
int set_r_attribute (AttributeSet *set, char *name, double dval);
int set_s_attribute (AttributeSet *set, char *name, char *sval);

/**
 * Get an attribute.  If no attribute has the given name, returns
 * an unpredictable value.
 */
char get_c_attribute (AttributeSet *set, char *name);
int get_d_attribute (AttributeSet *set, char *name);
int get_i_attribute (AttributeSet *set, char *name);
void *get_p_attribute (AttributeSet *set, char *name);
double get_r_attribute (AttributeSet *set, char *name);
char *get_s_attribute (AttributeSet *set, char *name);

/**
 * Determine if a set contains an attribute.
 */
int has_attribute (AttributeSet *set, char *name);

#endif // __ATTRIBUTE_H__ 
