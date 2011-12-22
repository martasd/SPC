
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 26 "pascal.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "attribute.h"
#include "parse-tree.h"
#include "symtab.h"
#include "stac.h"

/* Line 189 of yacc.c  */
#line 40 "pascal.y"

#define YYSTYPE Node *

/* Line 189 of yacc.c  */
#line 130 "pascal.y"

/* Nonterminals */
typedef enum nonterms
  {
    _addop,
    _array_type,
    _assignment_statement,
    _case_element,
    _case_element_list,
    _case_statement,
    _component_variable,
    _compound_statement,
    _cons,
    _constant,
    _constant_definition,
    _constant_definition_list,
    _constant_definition_part,
    _constant_list,
    _direction,
    _empty_set,
    _empty_statement,
    _entire_variable,
    _epsilon,
    _expr,
    _expr_list,
    _factor,
    _field_designator,
    _field_list,
    _file_type,
    _fixed_part,
    _formals,
    _formals_list,
    _formal_parameters,
    _for_statement,
    _function_call,
    _function_declaration,
    _function_heading,
    _function_parameter,
    _goto_statement,
    _id,
    _idlist,
    _if_then_else_statement,
    _if_statement,
    _if_then_statement,
    _indexed_variable,
    _label_declaration_part,
    _labeled_statement,
    _safe_statement,
    _mulop,
    _negate,
    _output_statement,
    _packed_structured_type,
    _parameter_group,
    _pointer_type,
    _procedure_call,
    _procedure_declaration,
    _procedure_declaration_list,
    _procedure_heading,
    _procedure_parameter,
    _program,
    _program_heading,
    _record_section,
    _record_section_list,
    _record_type,
    _referenced_variable,
    _relop,
    _repeat_statement,
    _repetitive_statement,
    _scalar_type,
    _scale_factor,
    _set,
    _set_type,
    _sign,
    _signed_identifier,
    _signed_number,
    _simple,
    _simple_statement,
    _simple_type,
    _simple_type_list,
    _statement,
    _statement_list,
    _statement_part,
    _string,
    _structured_statement,
    _structured_type,
    _subrange_type,
    _term,
    _type,
    _type_definition,
    _type_definition_list,
    _type_definition_part,
    _unlabeled_statement,
    _unpacked_structured_type,
    _unsigned_constant,
    _unsigned_integer,
    _unsigned_integer_list,
    _unsigned_number,
    _unsigned_real,
    _value_parameter,
    _variable,
    _variable_declaration,
    _variable_declaration_list,
    _variable_declaration_part,
    _variable_list,
    _variable_parameter,
    _variant,
    _variant_list,
    _variant_part,
    _while_statement,
    _write_parameter_list,
    _with_statement,
  } nonterms;

/* The names of the nonterminals. */
char *nonterm_names[] =
  {
    "addop",
    "array_type",
    "assignment_statement",
    "case_element",
    "case_element_list",
    "case_statement",
    "component_variable",
    "compound_statement",
    "cons",
    "constant",
    "constant_definition",
    "constant_definition_list",
    "constant_definition_part",
    "constant_list",
    "direction",
    "empty_set",
    "empty_statement",
    "entire_variable",
    "epsilon",
    "expr",
    "expr_list",
    "factor",
    "field_designator",
    "field_list",
    "file_type",
    "fixed_part",
    "formals",
    "formals_list",
    "formal_parameters",
    "for_statement",
    "function_call",
    "function_declaration",
    "function_heading",
    "function_parameter",
    "goto_statement",
    "id",
    "idlist",
    "if_then_else_statement",
    "if_statement",
    "if_then_statement",
    "indexed_variable",
    "label_declaration_part",
    "labeled_statement",
    "safe_statement",
    "mulop",
    "negate",
    "packed_structured_type",
    "parameter_group",
    "pointer_type",
    "procedure_call",
    "procedure_declaration",
    "procedure_declaration_list",
    "procedure_heading",
    "procedure_parameter",
    "program",
    "program_heading",
    "record_section",
    "record_section_list",
    "record_type",
    "referenced_variable",
    "relop",
    "repeat_statement",
    "repetitive_statement",
    "scalar_type",
    "scale_factor",
    "set",
    "set_type",
    "sign",
    "signed_identifier",
    "signed_number",
    "simple",
    "simple_statement",
    "simple_type",
    "simple_type_list",
    "statement",
    "statement_list",
    "statement_part",
    "string",
    "structured_statement",
    "structured_type",
    "subrange_type",
    "term",
    "type",
    "type_definition",
    "type_definition_list",
    "type_definition_part",
    "unlabeled_statement",
    "unpacked_structured_type",
    "unsigned_constant",
    "unsigned_integer",
    "unsigned_integer_list",
    "unsigned_number",
    "unsigned_real",
    "value_parameter",
    "variable",
    "variable_declaration",
    "variable_declaration_list",
    "variable_declaration_part",
    "variable_list",
    "variable_parameter",
    "variant",
    "variant_list",
    "variant_part",
    "while_statement",
    "with_statement",
  };

/* The names of the tokens. */
char *toknames[] =
  {
    "<tokens>",
    "*error*",
    "<general>", "ID", "</general>",
    "<constants>", "INT", "REAL", "STRING", "</constants>",
    "<keywords>", "ARRAY", "BEGIN", "CASE", "CONST", "DO",
    "DOWNTO", "ELSE", "END", "EOL", "FALSE", "FILE", "FOR", 
    "FUNCTION", "GOTO", "IF", "IN", "LABEL", "NIL", "OF", 
    "PACKED", "PROCEDURE", "PROGRAM", "RECORD", "REPEAT", 
    "SET", "THEN", "TO", "TRUE", "TYPE", "UNTIL", "VAR", 
    "WHILE", "WITH", "</keywords>",
    "<punctuation>", ":=", ":", ",", "..", "^", ";", "(", ")",
    "[", "]", ".", "</punctuation>",
    "<operators>", "NOT",
    "<mulops>", "AND", "DIV", "MOD", "/", "*", "</mulops>",
    "<addops>", "-", "OR", "+", "</addops>",
    "<relops>", "=", ">=", ">", "<=", "<", "<>", "</relops>",
    "</operators>",
    "</tokens>"
  };

/* Boo!  It seems that we have to define yyerror. */
void
yyerror (char *err)
{
  fprintf (stderr, "%s\n", err);
  // Give up.  We don't clean up memory b/c we're exiting
  exit (1);
} // yyerror


// +------------------+-----------------------------------------------
// | Useful variables |
// +------------------+

/* Agh!  We need to declare yytext b/c we use it below. It comes
 * from f?lex.
 */
extern char *yytext;

/* Our lovely parse tree. */
Node *tree = NULL;


// +----------------+-------------------------------------------------
// | Tree utilities |
// +----------------+

/**
 * Create a new interior node with as many children as you'd like.
 */
Node *
new_interior_node (int nonterm, AttributeSet *attributes, int arity, ...)
{
  va_list children;
  int c;
  Node *child;

  // Get ready to process varargs
  va_start (children, arity);

  // Create the new node.
  Node *node = new_nnode (nonterm, arity, attributes);

  // Add children.
  for (c= 0; c< arity; c++)
    {
      child = va_arg (children, Node *);
      set_child (node, c, child);
    } // for
  
  // Clean up from varargs.
  va_end (children);

  // And we're done
  return node;
} // new_interior_node
                   
/**
 * Create an epsilon node.
 * MARTIN: Changed the implementation because was causing segfault.
 */
Node *
new_epsilon ()
{
  AttributeSet *attributes = new_attribute_set (0);
  return new_interior_node (_epsilon, attributes, 0);
} // new_epsilon

/**
 * We seem to make a lot of simple_unary trees whose only child is
 * a leaf (see mulop and addop), so here's a helpful helper.
 */
static Node *
simple_unary_tree (int nonterm, int tok, AttributeSet *attributes)
{
  Node *node = new_nnode (nonterm, 1, attributes);
  set_child (node, 0, new_tnode (tok, new_attribute_set (0)));
  return node;
} // simple_unary_tree


// +----------------+-------------------------------------------------
// | Fun with lists |
// +----------------+

/**
 * Create a cons node (for lists).
 */
Node *
cons (Node *car, Node *cdr)
{
  return new_interior_node (_cons, NULL, 2, car, cdr);
} // cons

/**
 * Find the length of a list formed by cons cells.
 */
int 
list_length (Node *lst)
{
  int length = 0;
  while (lst->symbol != _epsilon)
    {
      lst = get_child (lst, 1);
      ++length;
    }
  return length;
} // list_length

/**
 * Given the car and cdr of a nonempty list, build a node that holds
 * all of the elements of the list.  Destructs the spine of the list
 * as it goes.
 */
Node *
list2node (int type, AttributeSet *attributes, Node *lst)
{
  int len = list_length (lst);      // The length of the total list
  Node *parent = new_nnode (type, len, attributes);
                                        // The node we're building
  Node *tmp;                            // Temporary node
  int child = 0;

  while (lst->symbol != _epsilon)
    {
      set_child (parent, child++, get_child (lst, 0));
      tmp = lst;
      lst = get_child (lst, 1);
      if (tmp->attributes != NULL)
        free_attribute_set (tmp->attributes);
      free (tmp);
    } // while
  if (lst->attributes != NULL)
    free_attribute_set (lst->attributes);
  free (lst);

  return parent;
} // list2node

/**
 * The normal mechanism for building a list is
 * list 
 *   : thing
 *     { $$ = $1; }
 *   | list SEP thing
 *     { $$ = cons ($1, $3); }
 *   ;
 * That builds a backwards, not-quite list structure.  The following
 * procedures help with such things (which I call "core"s).
 */

/**
 * Find out the number of elements in a core list.
 */
int
core_length (Node *core)
{
  int length = 1;
  while (core->symbol == _cons)
    {
      ++length;
      core = get_child (core, 0);
    } // while
  return length;
} // core_length

/**
 * The list is easier to use if we have a single node with all of those 
 * things as children (rather than descendents).  This procedure does the 
 * conversion, while eliminating the cons backbone we built.
 */
Node *
core2node (int symbol, AttributeSet *attributes, Node *core)
{
  int arity = core_length (core);
  Node *node = new_nnode (symbol, arity, attributes);
  Node *temp;
  int child = arity;
  while (core->symbol == _cons)
    {
      set_child (node, --child, get_child (core, 1));
      temp = core;
      core = get_child (core, 0);
    } // while
  set_child (node, 0, core);
  return node;
} // core2node

 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  Helper procedures for type checking.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

/* Returns the type of a terminal or nonterminal. */
TypeID
get_type_id (Node *node)
{
 Type *type = get_p_attribute (node->attributes, "type");
 return type->type_id;
}

/* Returns the amount of memory necessary to allocate for variable of type 
 * type_id.
 */
int
size_of_type (TypeID type_id)
{
  // THESE MAY NEED TO BE CHANGED!
  switch (type_id)
    {
    case (TYPE_BOOLEAN):
    case (TYPE_INTEGER):
      return 4;
    case (TYPE_REAL):
      return 4;
    case (TYPE_CHAR):
      return 1;
    case (TYPE_STRING):
      return 4;
    default:
      fprintf (stderr, "Variable does not have a type!\n");
      return 0;
    }
}

/* void */
/* create_type_att (TypeID type_id, AttributeSet attributes) */
/* { */
/*   Type *type = new_type (type_id); */
/*   type->type_id = type_id;  */
/*   switch (type_id) */
/*     { */
/*     case (TYPE_ARRAY): */
/*       type->info.array = ; */
/*     } */
/*   set_p_attribute (attributes, "type", type); */
/* } */

/* Creates and returns a type struct. */
Type *
new_type (TypeID type_id)
{
  struct Type *type = malloc (sizeof (struct Type));
  type->type_id = type_id; 
  return type;
}

/* Checks if two variables have the same type. If so, return 1, otherwise
 * return 0.
 */
int
types_compatible (Node *child0, Node *child1)
{
  /* Return 1 if the children have the same type. */
  if (get_type_id (child0) == get_type_id (child1))
    return 1;
  else 
    return 0; // Return zero if incompatible types. 
}

/* Miscelaneous helper procedures. */

 
/* Gets the operator. */
int
get_operator (Node *node)
{
 return get_child (node, 0)->symbol;
}

int
get_arity (Node *node)
{
  if (! is_nnode (node))
    return 0;
  NNode *nn = (NNode *) node;
  return nn->arity;
} // get_arity

// STUB
Type *get_params(Node *node)
{
 return 0;
}

/**
 * A quick hack to support debugging.
 */
void 
stop_here (void)
{
}
 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  Keeping track of formal parameters in functions and procedures
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 
// Maximum number of parameters in a function or procedure
#define MAX_PARAMS 128

// Store parameters in a global array
Param params[MAX_PARAMS];

// Initially, there are no parameters
int num_params = 0;

void 
clear_params ()
{
 num_params = 0;
}

int
get_num_params ()
{
 return num_params;
}

// Store the next parameter in the array of params
void
insert_param (char *name, Type *type)
{
 params[num_params].name = name;
 params[num_params].type = type;
 num_params++;
}


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  Other useful functions
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


/* Retrieve the type of the formal parameter at index i. */
Type *
get_param_type (Type *type, int i)
{
  return type->info.function_procedure->params[i].type;
}

/* Retrieve the name of the formal parameter at index i. */
char *
get_param_name (Type *type, int i)
{
  return type->info.function_procedure->params[i].name;
}

/* Construct function and procedure type. */
Type *
function_procedure_type (Type *return_type, int num_params)
{

  Type *type;
  
  /* Allocate a struct for proc attributes. */
  struct FunctionProcedureType *func_proc_struct = 
    malloc (sizeof (struct FunctionProcedureType));
            

  func_proc_struct->num_params = num_params;
      
  /* Determine size of formal parameters. */
  int params_memsize = sizeof (struct Param) * num_params; 

  /* Allocate space for formal parameters. */ 
  func_proc_struct->params = malloc (params_memsize);
  if ((func_proc_struct->params == NULL) && (num_params != 0))
    {
      fprintf (stderr, "Could not allocate space for parameters!\n");
      free (func_proc_struct);
      return NULL;
    }

  /* Copy params from global array to proc->struct. */
  memcpy (func_proc_struct->params, params, params_memsize);
      
  /* Construct a type for function or procedure */
  if (return_type ==NULL)
    type = new_type (TYPE_PROCEDURE);
  else
    type = new_type (TYPE_FUNCTION);
    
  type->info.function_procedure = func_proc_struct;
  return type;
}


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Dealing with temporary variables
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define MAX_TEMPS 100

/* A temporary can be an integer, a real, a char, or a string. */
typedef union Temp
{
  int integer;
  float real;
  char character;
  char *string;
} Temp;

/* An array to store temporaries for the generation of three-address code. */
Temp temps[MAX_TEMPS];

/* Keep track of size and capacity fo the temps array. */
int temps_capacity = MAX_TEMPS;
int num_temps = 0;

/* Create a new temporary, add it to the array, and return its array index. */
int
new_temp ()
{
  int i, temp_index;

  if (num_temps >= temps_capacity)
    {
      /* Double the array of temporaries and insert the next one. */
      Temp new_temps[temps_capacity * 2];
      /* If no more space available, then throw error. */
      if (new_temps == NULL)
        {
          fprintf (stderr, "No more memory can be allocated for temporararies!\n");
          return 0;
        }
      
      /* Copy the temporaries over. */
      for (i = 0; i < num_temps; i++)
        new_temps[i] = temps[i];

      temps_capacity = temps_capacity * 2;
    }
  
  temp_index = num_temps - 1;
  num_temps++;
  return temp_index;
}

/* Clear the array of temporaries once we do not need them anymore. */
void
clear_temps ()
{
  num_temps = 0;
}

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Dealing with activation records
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

/* Do not allow nesting deeper than 128. */
#define MAX_ACT_RECORDS 128
 
/* The stack stack of activation records, which is equivalent to stack of hash tables
 * in the symbol table. The stack stores the amount of memory that an activation
 * record requires.
 */
int activation_records[MAX_ACT_RECORDS];
 
/* Initially, there is no activation record. */
int num_activation_records = 0;


/* Start a new activation record. */
int
ar_enter ()
{
  /* If too many act. records, then report an error. */
  if (num_activation_records >= MAX_ACT_RECORDS)
    {
      fprintf (stderr, "Nesting of functions/procedures is too deep!\n");
      fprintf (stderr, "Cannot allocate another activation record.\n");
      return 0;
    }

  /* Initialize temporaries. */
  clear_temps ();
  
  num_activation_records++;

  /* Allocate space for things present by default in every ar: 4 bytes for
   * previous frame pointer and 4 bytes for return address. */
  activation_records[num_activation_records-1] = 8;
  return 1;
} // ar_enter

/* Exit from the current activation record. */
int
ar_exit ()
{
  /* If there are no activation records, then we cannot pop. */
  if (num_activation_records == 0)
    {
      fprintf (stderr, "This is the only activation record, so cannot leave it!\n");
      return 0;
    }

  num_activation_records--;
  return 1;
} // ar_exit

/* Allocate amount of space in current activation record and return offset. */
int
ar_alloc (int amount)
{
  int offset = -activation_records[num_activation_records-1];
  activation_records[num_activation_records-1] += amount;
  return offset;
} // ar_alloc

/* Determine how much space has been allocated in current activation record. */
int
ar_total ()
{
  return activation_records[num_activation_records-1];
} // ar_total

/* Store activation record size in the symbol table. */
int
ar_store_size ()
{
  
  return 1;
}

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Functions related for generating instructions
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#define MAX_INSTRUCTIONS 1024

/* A global array to hold generated instructions. Needs to be allocated. */
Instruction *instructions;

/* Keep track of size and capacity of the instructions array. */
int max_instructions = MAX_INSTRUCTIONS;
int num_instructions = 0;

/* Add an instruction to the array of instructions. */
int
generate_instruction (OpCode opcode, StacParameter *param1, StacParameter *param2, StacParameter *param3)
{
  /* If the array is full, then double it. */
  if (num_instructions >= max_instructions)
    {
      /* Double the space allocated for instructions array. */
      Instruction *temp = 
	realloc (instructions, sizeof (Instruction) * max_instructions * 2);

      /* If no more memory can be allocated, then throw an error. */
      if (temp == NULL)
	{
	  fprintf (stderr, "No more memory can be allocated for instructions!\n");
	  return 0;
	}

      instructions = temp;
      max_instructions = max_instructions * 2;
    }
  
  /* Save the instruction into the array. */
  build_instruction (&(instructions[num_instructions]), opcode, param1, param2, param3);
  num_instructions++;
  return 1;
}

/* Wrapper for generate_instruction, so that the generation of code for
   statement can be postponed till we percolate up to the appropriate
   parent. */
void
generate_statement (Instruction *instruction)
{
  generate_instruction (instruction->op, instruction->params[0],
                        instruction->params[1], instruction->params[2]);
}

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Labels
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

int num_labels = 0;

#define LABEL_LENGTH 20

typedef enum Label
  {
    CONSEQUENT_LABEL,
    ALTERNATE_LABEL,
    END_LABEL
  } Label;

StacParameter *consequent_label = malloc (sizeof (StacParameter));
StacParameter *alternate_label = malloc (sizeof (StacParameter));
StacParameter *end_label = malloc (sizeof (StacParameter));

/* VERY SUBOPTIMAL: REMEMBER TO IMPROVE DESIGN OF LABEL */
/* Generate next unique label. */
void
generate_end_label (Label label_type)
{
  /* Allocate space for the next label. */
  char *label = malloc (sizeof (char) * LABEL_LENGTH);

  /* Store the label number as a string. */
  sprintf (label, "%d", num_labels);

  switch (label_type)
    {
    case (CONSEQUENT_LABEL):
      consequent_label->type = LABEL;
      consequent_label->info.s = label;
    case (ALTERNATE_LABEL):
      alternate_label->type = LABEL;
      alternate_label->info.s = label;
    case (END_LABEL):
      end_label->type = LABEL;
      end_label->info.s = label;
    default:
      fprintf (stderr, "Passed unrecognized label type.\n");
    }
   
  num_labels++;
}  

// StacParameter *
// bad_generate_label ()
// {
//   /* Double the array of labels if it is already full. */
//   if ( num_labels >= max_labels)
//     {
//       StacParameter *temp =
// 	realloc (labels, sizeof (Label) * max_labels * 2);
//       
//       /* If no more memory can be allocated, then throw an error. */
//       if (temp == NULL)
// 	{
// 	  fprintf (stderr, "No more memory can be allocated for labels!\n");
// 	  return 0;
// 	} 
//       
//       labels = temp;
//       max_labels = max_labels * 2;
//     }
//   
//   /* Allocate space for the next label. */
//   char *label;
//   label = malloc (sizeof (char) * LABEL_LENGTH);
// 
//   /* Store the label number as a string. */
//   sprintf (label, "%d", num_labels);
// 
//   /* Put the label in the array. */
//   labels[num_labels]->type = LABEL;
//   labels[num_labels]->info.s = label;
//   num_labels++;
//   return label_param;
// }


/* Copied from pascal.y by Sam Rebelsky. */
OpCode get_arithmetic_opcode (int operator, TypeID type_id);
OpCode get_boolean_opcode (int operator, TypeID type_id);
OpCode get_assignment_opcode (TypeID type_id);
int is_arithmetic_operator (int operator);
int is_boolean_operator (int operator);
int is_assignment_operator (int operator);
OpCode get_opcode (int operator, TypeID operand);
StacParameter *translate_expr (int operator, Node *left, Node *right);
void translate_bexp (Node *node, StacParameter *truelabel, StacParameter *falselabel);

/* STUB */
void
init_symtab ()
{
}

/* Declare the symbol table for the program. */
SymTab *stab;


/* Line 189 of yacc.c  */
#line 1017 "pascal.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOKENS_START = 258,
     _ERROR = 259,
     GENERAL_START = 260,
     _IDENTIFIER = 261,
     GENERAL_END = 262,
     CONSTANTS_START = 263,
     _INTEGER = 264,
     _REAL = 265,
     _STRING = 266,
     CONSTANTS_END = 267,
     KEYWORDS_START = 268,
     _ARRAY = 269,
     _BEGIN = 270,
     _CASE = 271,
     _CONST = 272,
     _DO = 273,
     _DOWNTO = 274,
     _ELSE = 275,
     _END = 276,
     _EOL = 277,
     _FALSE = 278,
     _FILE = 279,
     _FOR = 280,
     _FUNCTION = 281,
     _GOTO = 282,
     _IF = 283,
     _IN = 284,
     _LABEL = 285,
     _NIL = 286,
     _OF = 287,
     _PACKED = 288,
     _WRITE = 289,
     _WRITELN = 290,
     _PROCEDURE = 291,
     _PROGRAM = 292,
     _RECORD = 293,
     _REPEAT = 294,
     _SET = 295,
     _THEN = 296,
     _TO = 297,
     _TRUE = 298,
     _TYPE = 299,
     _UNTIL = 300,
     _VAR = 301,
     _WHILE = 302,
     _WITH = 303,
     KEYWORDS_END = 304,
     PUNCTUATION_START = 305,
     _ASSIGN = 306,
     _COLON = 307,
     _COMMA = 308,
     _ELLIPSES = 309,
     _POINTER = 310,
     _SEMICOLON = 311,
     _LPAREN = 312,
     _RPAREN = 313,
     _LBRACKET = 314,
     _RBRACKET = 315,
     _DOT = 316,
     PUNCTUATION_END = 317,
     OPERATORS_START = 318,
     _NOT = 319,
     MULOPS_START = 320,
     _AND = 321,
     _DIV = 322,
     _MOD = 323,
     _SLASH = 324,
     _STAR = 325,
     MULOPS_END = 326,
     ADDOPS_START = 327,
     _DASH = 328,
     _OR = 329,
     _PLUS = 330,
     ADDOPS_END = 331,
     RELOPS_START = 332,
     _EQ = 333,
     _GE = 334,
     _GT = 335,
     _LE = 336,
     _LT = 337,
     _NE = 338,
     RELOPS_END = 339,
     OPERATORS_END = 340,
     TOKENS_END = 341
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 1145 "pascal.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   679

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  87
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  132
/* YYNRULES -- Number of rules.  */
#define YYNRULES  227
/* YYNRULES -- Number of states.  */
#define YYNSTATES  414

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   341

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    12,    13,    17,    19,
      21,    23,    25,    27,    29,    31,    33,    35,    37,    39,
      41,    44,    47,    49,    51,    53,    55,    57,    60,    61,
      65,    69,    71,    73,    75,    79,    81,    83,    85,    89,
      92,    93,    97,   101,   103,   105,   108,   110,   112,   114,
     116,   123,   126,   127,   131,   135,   137,   140,   142,   143,
     146,   150,   157,   163,   165,   168,   169,   173,   177,   181,
     184,   188,   190,   192,   194,   197,   198,   202,   204,   206,
     208,   213,   217,   220,   222,   224,   226,   228,   232,   235,
     239,   242,   246,   248,   252,   255,   257,   261,   263,   266,
     267,   271,   273,   275,   277,   279,   281,   283,   285,   287,
     289,   291,   293,   295,   297,   299,   301,   305,   310,   313,
     314,   318,   320,   322,   324,   326,   328,   330,   334,   338,
     340,   342,   344,   346,   348,   350,   352,   354,   358,   363,
     368,   371,   372,   376,   378,   380,   385,   388,   389,   391,
     393,   395,   397,   399,   401,   403,   407,   409,   411,   413,
     415,   416,   422,   429,   436,   442,   446,   449,   450,   454,
     456,   458,   460,   462,   464,   469,   474,   479,   488,   497,
     499,   501,   506,   511,   519,   526,   527,   529,   531,   533,
     535,   537,   540,   541,   545,   547,   550,   553,   556,   560,
     561,   565,   566,   569,   573,   574,   578,   579,   582,   586,
     587,   591,   592,   595,   599,   600,   604,   605,   609,   610,
     614,   616,   618,   626,   635,   644,   645,   649
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      88,     0,    -1,    -1,    89,   217,    -1,     9,    -1,    90,
      92,    -1,    -1,    53,    90,    92,    -1,    10,    -1,    90,
      -1,    93,    -1,    75,    -1,    73,    -1,    11,    -1,     6,
      -1,    94,    -1,    96,    -1,    31,    -1,    43,    -1,    23,
      -1,    95,    94,    -1,    95,    97,    -1,    94,    -1,    99,
      -1,    97,    -1,   100,    -1,    96,    -1,   101,   103,    -1,
      -1,    53,   101,   103,    -1,    97,    78,   101,    -1,   107,
      -1,   112,    -1,   129,    -1,    97,    78,   105,    -1,   108,
      -1,   111,    -1,    97,    -1,    57,   109,    58,    -1,    97,
     110,    -1,    -1,    53,    97,   110,    -1,   101,    54,   101,
      -1,   114,    -1,   113,    -1,    33,   114,    -1,   115,    -1,
     118,    -1,   127,    -1,   128,    -1,    14,    59,   116,    60,
      32,   105,    -1,   107,   117,    -1,    -1,    53,   107,   117,
      -1,    38,   119,    21,    -1,   120,    -1,   122,   121,    -1,
     123,    -1,    -1,    56,   120,    -1,   109,    52,   105,    -1,
      16,    97,    52,    97,    32,   125,    -1,   102,    52,    57,
     119,    58,    -1,   102,    -1,   124,   126,    -1,    -1,    56,
     124,   126,    -1,    40,    32,   107,    -1,    24,    32,   105,
      -1,    55,    97,    -1,   109,    52,   105,    -1,   134,    -1,
     135,    -1,   138,    -1,   131,   133,    -1,    -1,    53,   131,
     133,    -1,    97,    -1,   136,    -1,   137,    -1,   131,    59,
     144,    60,    -1,   131,    61,    97,    -1,   131,    55,    -1,
     131,    -1,    98,    -1,   149,    -1,   140,    -1,    57,   143,
      58,    -1,    64,   139,    -1,    59,   144,    60,    -1,    59,
      60,    -1,   141,   146,   139,    -1,   139,    -1,   142,   147,
     141,    -1,   147,   141,    -1,   141,    -1,   142,   148,   142,
      -1,   142,    -1,   143,   145,    -1,    -1,    53,   143,   145,
      -1,    70,    -1,    69,    -1,    67,    -1,    68,    -1,    66,
      -1,    75,    -1,    73,    -1,    74,    -1,    82,    -1,    81,
      -1,    78,    -1,    83,    -1,    79,    -1,    80,    -1,    29,
      -1,    97,    57,    58,    -1,    97,    57,   144,    58,    -1,
     152,   151,    -1,    -1,    56,   152,   151,    -1,   153,    -1,
     154,    -1,   155,    -1,   157,    -1,   156,    -1,   158,    -1,
      90,    52,   157,    -1,    90,    52,   158,    -1,   159,    -1,
     168,    -1,   169,    -1,   160,    -1,   161,    -1,   165,    -1,
     166,    -1,   167,    -1,   131,    51,   143,    -1,    34,    57,
     162,    58,    -1,    35,    57,   162,    58,    -1,   164,   163,
      -1,    -1,    53,   164,   163,    -1,   143,    -1,    97,    -1,
      97,    57,   144,    58,    -1,    27,    90,    -1,    -1,   170,
      -1,   171,    -1,   181,    -1,   189,    -1,   172,    -1,   182,
      -1,   190,    -1,    15,   150,    21,    -1,   175,    -1,   177,
      -1,   173,    -1,   176,    -1,    -1,    28,   143,   174,    41,
     153,    -1,    28,   143,    41,   153,    20,   153,    -1,    28,
     143,    41,   153,    20,   154,    -1,    16,   143,    32,   179,
      21,    -1,   102,    52,   152,    -1,   178,   180,    -1,    -1,
      56,   178,   180,    -1,   183,    -1,   185,    -1,   186,    -1,
     184,    -1,   187,    -1,    47,   143,    18,   153,    -1,    47,
     143,    18,   154,    -1,    39,   150,    45,   143,    -1,    25,
      97,    51,   143,   188,   143,    18,   153,    -1,    25,    97,
      51,   143,   188,   143,    18,   154,    -1,    42,    -1,    19,
      -1,    48,   132,    18,   153,    -1,    48,   132,    18,   154,
      -1,   192,   202,   203,   206,   209,   212,   170,    -1,    36,
      97,    57,   193,    58,    56,    -1,    -1,   195,    -1,   197,
      -1,   198,    -1,   199,    -1,   200,    -1,   194,   196,    -1,
      -1,    56,   194,   196,    -1,   201,    -1,    46,   201,    -1,
      26,   201,    -1,    36,   109,    -1,   109,    52,    97,    -1,
      -1,    30,    91,    56,    -1,    -1,    17,   204,    -1,   104,
      56,   205,    -1,    -1,   104,    56,   205,    -1,    -1,    44,
     207,    -1,   106,    56,   208,    -1,    -1,   106,    56,   208,
      -1,    -1,    46,   210,    -1,   130,    56,   211,    -1,    -1,
     130,    56,   211,    -1,    -1,   214,    56,   213,    -1,    -1,
     214,    56,   213,    -1,   191,    -1,   215,    -1,   216,   202,
     203,   206,   209,   212,   170,    -1,    26,    97,    57,   193,
      58,    52,    97,    56,    -1,   218,   202,   203,   206,   209,
     212,   170,    61,    -1,    -1,    37,    97,    56,    -1,    37,
      97,    57,   109,    58,    56,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1063,  1063,  1063,  1106,  1124,  1133,  1134,  1141,  1159,
    1160,  1164,  1166,  1171,  1191,  1208,  1210,  1212,  1214,  1216,
    1221,  1229,  1237,  1239,  1241,  1243,  1245,  1250,  1259,  1260,
    1267,  1283,  1285,  1287,  1292,  1312,  1314,  1316,  1336,  1344,
    1353,  1354,  1363,  1416,  1418,  1423,  1431,  1433,  1435,  1437,
    1444,  1482,  1491,  1492,  1499,  1520,  1531,  1536,  1544,  1549,
    1554,  1562,  1570,  1575,  1583,  1592,  1593,  1600,  1610,  1620,
    1630,  1672,  1674,  1676,  1681,  1690,  1691,  1698,  1725,  1727,
    1734,  1744,  1759,  1769,  1771,  1773,  1775,  1777,  1779,  1792,
    1797,  1805,  1851,  1858,  1897,  1909,  1914,  1943,  1948,  1957,
    1958,  1969,  1974,  1979,  1984,  1989,  1999,  2004,  2009,  2019,
    2024,  2029,  2034,  2039,  2044,  2049,  2063,  2068,  2086,  2095,
    2096,  2101,  2103,  2108,  2110,  2115,  2117,  2122,  2132,  2140,
    2142,  2147,  2154,  2156,  2158,  2160,  2162,  2174,  2219,  2259,
    2281,  2290,  2291,  2296,  2303,  2308,  2318,  2329,  2338,  2340,
    2342,  2344,  2349,  2351,  2353,  2360,  2367,  2369,  2374,  2376,
    2384,  2383,  2411,  2443,  2477,  2485,  2493,  2502,  2503,  2510,
    2512,  2514,  2519,  2521,  2528,  2536,  2546,  2556,  2565,  2574,
    2579,  2589,  2597,  2607,  2622,  2677,  2678,  2683,  2685,  2687,
    2689,  2694,  2703,  2704,  2709,  2717,  2725,  2733,  2741,  2774,
    2776,  2785,  2787,  2793,  2802,  2803,  2808,  2809,  2814,  2823,
    2824,  2830,  2831,  2836,  2845,  2846,  2852,  2853,  2862,  2863,
    2868,  2870,  2877,  2892,  2902,  2919,  2923,  2928
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOKENS_START", "_ERROR",
  "GENERAL_START", "_IDENTIFIER", "GENERAL_END", "CONSTANTS_START",
  "_INTEGER", "_REAL", "_STRING", "CONSTANTS_END", "KEYWORDS_START",
  "_ARRAY", "_BEGIN", "_CASE", "_CONST", "_DO", "_DOWNTO", "_ELSE", "_END",
  "_EOL", "_FALSE", "_FILE", "_FOR", "_FUNCTION", "_GOTO", "_IF", "_IN",
  "_LABEL", "_NIL", "_OF", "_PACKED", "_WRITE", "_WRITELN", "_PROCEDURE",
  "_PROGRAM", "_RECORD", "_REPEAT", "_SET", "_THEN", "_TO", "_TRUE",
  "_TYPE", "_UNTIL", "_VAR", "_WHILE", "_WITH", "KEYWORDS_END",
  "PUNCTUATION_START", "_ASSIGN", "_COLON", "_COMMA", "_ELLIPSES",
  "_POINTER", "_SEMICOLON", "_LPAREN", "_RPAREN", "_LBRACKET", "_RBRACKET",
  "_DOT", "PUNCTUATION_END", "OPERATORS_START", "_NOT", "MULOPS_START",
  "_AND", "_DIV", "_MOD", "_SLASH", "_STAR", "MULOPS_END", "ADDOPS_START",
  "_DASH", "_OR", "_PLUS", "ADDOPS_END", "RELOPS_START", "_EQ", "_GE",
  "_GT", "_LE", "_LT", "_NE", "RELOPS_END", "OPERATORS_END", "TOKENS_END",
  "$accept", "start", "$@1", "unsigned_integer", "unsigned_integer_list",
  "unsigned_integer_list_tail", "unsigned_real", "unsigned_number", "sign",
  "string", "id", "unsigned_constant", "signed_number",
  "signed_identifier", "constant", "constant_list", "constant_list_tail",
  "constant_definition", "type", "type_definition", "simple_type",
  "scalar_type", "idlist", "idtail", "subrange_type", "structured_type",
  "packed_structured_type", "unpacked_structured_type", "array_type",
  "simple_type_list", "simple_type_list_tail", "record_type", "field_list",
  "field_list_kernel", "field_list_tail", "record_section", "variant_part",
  "variant", "variant_list", "variant_list_tail", "set_type", "file_type",
  "pointer_type", "variable_declaration", "variable", "variable_list",
  "variable_list_tail", "entire_variable", "component_variable",
  "indexed_variable", "field_designator", "referenced_variable", "factor",
  "set", "term", "simple", "expr", "expr_list", "expr_list_tail", "mulop",
  "addop", "relop", "function_call", "statement_list",
  "statement_list_tail", "statement", "safe_statement", "unsafe_statement",
  "labeled_safe_statement", "labeled_unsafe_statement",
  "unlabeled_safe_statement", "unlabeled_unsafe_statement",
  "simple_statement", "assignment_statement", "output_statement",
  "write_parameter_list", "write_parameter_list_tail", "write_parameter",
  "procedure_call", "goto_statement", "empty_statement",
  "safe_structured_statement", "unsafe_structured_statement",
  "compound_statement", "safe_conditional_statement",
  "unsafe_conditional_statement", "if_then_statement", "$@2",
  "safe_if_then_else_statement", "unsafe_if_then_else_statement",
  "case_statement", "case_element", "case_element_list",
  "case_element_list_tail", "safe_repetitive_statement",
  "unsafe_repetitive_statement", "safe_while_statement",
  "unsafe_while_statement", "repeat_statement", "safe_for_statement",
  "unsafe_for_statement", "direction", "safe_with_statement",
  "unsafe_with_statement", "procedure_declaration", "procedure_heading",
  "formal_parameters", "formals", "formals_list", "formals_list_tail",
  "value_parameter", "variable_parameter", "function_parameter",
  "procedure_parameter", "parameter_group", "label_declaration_part",
  "constant_definition_part", "constant_definition_list",
  "constant_definition_list_tail", "type_definition_part",
  "type_definition_list", "type_definition_list_tail",
  "variable_declaration_part", "variable_declaration_list",
  "variable_declaration_list_tail", "procedure_declaration_list",
  "procedure_declaration_list_tail", "procedure_or_function_declaration",
  "function_declaration", "function_heading", "program", "program_heading", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    87,    89,    88,    90,    91,    92,    92,    93,    94,
      94,    95,    95,    96,    97,    98,    98,    98,    98,    98,
      99,   100,   101,   101,   101,   101,   101,   102,   103,   103,
     104,   105,   105,   105,   106,   107,   107,   107,   108,   109,
     110,   110,   111,   112,   112,   113,   114,   114,   114,   114,
     115,   116,   117,   117,   118,   119,   120,   120,   121,   121,
     122,   123,   124,   124,   125,   126,   126,   127,   128,   129,
     130,   131,   131,   131,   132,   133,   133,   134,   135,   135,
     136,   137,   138,   139,   139,   139,   139,   139,   139,   140,
     140,   141,   141,   142,   142,   142,   143,   143,   144,   145,
     145,   146,   146,   146,   146,   146,   147,   147,   147,   148,
     148,   148,   148,   148,   148,   148,   149,   149,   150,   151,
     151,   152,   152,   153,   153,   154,   154,   155,   156,   157,
     157,   158,   159,   159,   159,   159,   159,   160,   161,   161,
     162,   163,   163,   164,   165,   165,   166,   167,   168,   168,
     168,   168,   169,   169,   169,   170,   171,   171,   172,   172,
     174,   173,   175,   176,   177,   178,   179,   180,   180,   181,
     181,   181,   182,   182,   183,   184,   185,   186,   187,   188,
     188,   189,   190,   191,   192,   193,   193,   194,   194,   194,
     194,   195,   196,   196,   197,   198,   199,   200,   201,   202,
     202,   203,   203,   204,   205,   205,   206,   206,   207,   208,
     208,   209,   209,   210,   211,   211,   212,   212,   213,   213,
     214,   214,   215,   216,   217,   218,   218,   218
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     2,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     2,     0,     3,
       3,     1,     1,     1,     3,     1,     1,     1,     3,     2,
       0,     3,     3,     1,     1,     2,     1,     1,     1,     1,
       6,     2,     0,     3,     3,     1,     2,     1,     0,     2,
       3,     6,     5,     1,     2,     0,     3,     3,     3,     2,
       3,     1,     1,     1,     2,     0,     3,     1,     1,     1,
       4,     3,     2,     1,     1,     1,     1,     3,     2,     3,
       2,     3,     1,     3,     2,     1,     3,     1,     2,     0,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     2,     0,
       3,     1,     1,     1,     1,     1,     1,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     4,
       2,     0,     3,     1,     1,     4,     2,     0,     1,     1,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     1,
       0,     5,     6,     6,     5,     3,     2,     0,     3,     1,
       1,     1,     1,     1,     4,     4,     4,     8,     8,     1,
       1,     4,     4,     7,     6,     0,     1,     1,     1,     1,
       1,     2,     0,     3,     1,     2,     2,     2,     3,     0,
       3,     0,     2,     3,     0,     3,     0,     2,     3,     0,
       3,     0,     2,     3,     0,     3,     0,     3,     0,     3,
       1,     1,     7,     8,     8,     0,     3,     6
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,   225,     1,     0,     3,   199,    14,     0,     0,
     201,   226,     0,     4,     6,     0,     0,   206,    40,     0,
       0,     5,   200,     0,     0,   202,     0,   211,     0,    39,
       0,     6,     0,   204,     0,     0,   207,     0,   216,    40,
     227,     7,     8,    13,    12,    11,     9,    10,    22,     0,
      26,    24,    23,    25,    30,     0,   203,     0,   209,     0,
       0,   212,     0,     0,   220,   199,     0,     0,   221,   199,
      41,    20,    21,   204,     0,     0,     0,     0,     0,     0,
       0,    37,     0,    34,    31,    35,    36,    32,    44,    43,
      46,    47,    48,    49,    33,     0,   208,     0,   214,     0,
       0,   201,   147,     0,   218,   201,   205,     0,     0,    45,
       0,     0,     0,    55,    58,    57,     0,    69,     0,     0,
     209,    70,     0,   213,   185,   185,   206,     0,     0,     0,
       0,     0,     0,   147,     0,     0,     0,    77,     0,    71,
      72,    78,    79,    73,     0,   119,   121,   122,   123,   125,
     124,   126,   129,   132,   133,   134,   135,   136,   130,   131,
     148,   149,   152,   158,   156,   159,   157,   150,   153,   169,
     172,   170,   171,   173,   151,   154,   224,   217,     0,   206,
      52,     0,    68,     0,     0,    54,     0,    56,    67,    38,
      42,   210,   214,     0,     0,     0,     0,     0,   192,   186,
     187,   188,   189,   190,   194,     0,   211,    19,    17,    18,
       0,     0,     0,   107,   108,   106,    15,    16,    77,    84,
      83,    92,    86,    95,    97,     0,     0,    85,     0,   146,
       0,     0,     0,     0,     0,    77,    75,     0,   147,     0,
       0,    82,     0,     0,   155,   147,   118,   218,   211,     0,
      51,     0,     0,    60,    59,   215,   196,   197,   195,     0,
       0,     0,   191,     0,   216,     0,    90,    99,     0,    88,
       0,   105,   103,   104,   102,   101,     0,   115,   111,   113,
     114,   110,   109,   112,     0,     0,     0,    94,     0,   147,
       0,   143,     0,   141,     0,     0,   147,     0,    74,   147,
     127,   128,     0,   137,     0,    81,   119,   219,   216,    52,
       0,     0,   198,     0,   192,   184,     0,    87,     0,    98,
      89,   116,     0,    91,    93,    96,    28,     0,   167,     0,
       0,     0,     0,     0,     0,     0,     0,   147,   138,     0,
     140,   139,   176,   174,   175,    75,   181,   182,   145,    80,
     120,     0,    53,    50,     0,     0,   193,   183,    99,   117,
       0,    27,   147,     0,   166,   164,   180,   179,     0,     0,
       0,     0,     0,   147,   147,   161,   141,    76,   222,    63,
      65,    61,   223,   100,    28,   165,   167,     0,     0,   147,
     147,   147,   162,   163,   142,     0,     0,    64,    29,   168,
     147,     0,     0,     0,    65,   177,   178,     0,   147,     0,
      66,     0,    62,   147
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    46,    15,    21,    47,   216,    49,   217,
     218,   219,    52,    53,    82,   327,   361,    55,    83,    95,
      84,    85,   196,    29,    86,    87,    88,    89,    90,   181,
     250,    91,   112,   113,   187,   114,   115,   380,   381,   397,
      92,    93,    94,   122,   220,   237,   298,   139,   140,   141,
     142,   143,   221,   222,   223,   224,   267,   268,   319,   276,
     226,   285,   227,   144,   246,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,   292,   340,   293,   155,   156,
     157,   158,   159,   160,   161,   162,   163,   290,   164,   165,
     166,   328,   329,   364,   167,   168,   169,   170,   171,   172,
     173,   368,   174,   175,    64,    65,   197,   198,   199,   262,
     200,   201,   202,   203,   204,    10,    17,    25,    56,    27,
      36,    96,    38,    61,   123,    66,   177,    67,    68,    69,
       5,     6
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -335
static const yytype_int16 yypact[] =
{
    -335,    21,    -2,  -335,    71,  -335,    10,  -335,    26,    77,
      63,  -335,    71,  -335,    46,    45,    71,    58,    57,    55,
      77,  -335,  -335,    36,    69,  -335,    71,    73,    71,  -335,
      78,    46,   153,    71,    49,    79,  -335,    71,     6,    57,
    -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,    42,
    -335,  -335,  -335,  -335,  -335,    83,  -335,   453,    71,    89,
      92,  -335,    71,    71,  -335,    10,   137,    97,  -335,    10,
    -335,  -335,  -335,    71,    96,   125,    17,    40,   126,    71,
      71,   102,   107,  -335,  -335,  -335,  -335,  -335,  -335,  -335,
    -335,  -335,  -335,  -335,  -335,   110,  -335,   453,    71,   114,
     128,    63,   561,   131,     6,    63,  -335,   127,   453,  -335,
      71,   135,   174,  -335,   140,  -335,   127,  -335,   139,   153,
      71,  -335,   142,  -335,   124,   124,    58,   509,    71,    77,
     509,   147,   148,   561,   509,    71,   160,   158,   -12,  -335,
    -335,  -335,  -335,  -335,   192,   161,  -335,  -335,  -335,  -335,
    -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,
    -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,
    -335,  -335,  -335,  -335,  -335,  -335,  -335,  -335,   162,    58,
     168,   169,  -335,   180,   453,  -335,    40,  -335,  -335,  -335,
    -335,  -335,    71,    71,    71,    71,   181,   177,   184,  -335,
    -335,  -335,  -335,  -335,  -335,   185,    73,  -335,  -335,  -335,
     509,   163,   528,  -335,  -335,  -335,  -335,  -335,   187,  -335,
      67,  -335,  -335,   141,   401,   210,   528,  -335,   195,  -335,
     206,   509,   509,   204,   232,  -335,   122,   234,   604,   509,
     509,  -335,   509,    71,  -335,   561,  -335,     6,    73,   127,
    -335,   221,    71,  -335,  -335,  -335,  -335,  -335,  -335,    71,
     207,   124,  -335,   208,     6,   202,  -335,   209,   205,  -335,
     430,  -335,  -335,  -335,  -335,  -335,   528,  -335,  -335,  -335,
    -335,  -335,  -335,  -335,   528,   509,   153,   141,   509,   588,
     225,  -335,   212,   218,   214,   509,   561,    71,  -335,   561,
    -335,  -335,   215,  -335,   216,  -335,   161,  -335,     6,   168,
     453,   242,  -335,    71,   184,  -335,   137,  -335,   509,  -335,
    -335,  -335,   219,  -335,   141,    34,   226,   228,   222,   260,
       8,    71,   509,   509,    71,   231,   267,   588,  -335,   509,
    -335,  -335,  -335,  -335,  -335,   122,  -335,  -335,  -335,  -335,
    -335,   137,  -335,  -335,   153,   233,  -335,  -335,   209,  -335,
     153,  -335,   561,   153,  -335,  -335,  -335,  -335,   509,   237,
     249,   276,   278,   631,   561,  -335,   218,  -335,  -335,   245,
     243,  -335,  -335,  -335,   226,  -335,   222,   280,   509,   588,
     588,   588,  -335,  -335,  -335,   244,   153,  -335,  -335,  -335,
     561,     8,   282,    40,   243,  -335,  -335,   509,   588,   250,
    -335,   285,  -335,   588
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -335,  -335,  -335,    16,  -335,   269,  -335,   -19,  -335,    35,
      -4,  -335,  -335,  -335,   -23,  -334,   -77,   294,   -94,   287,
    -105,  -335,     7,   272,  -335,  -335,  -335,   238,  -335,  -335,
       9,  -335,   -87,   133,  -335,  -335,  -335,   -79,  -335,   -84,
    -335,  -335,  -335,   286,   123,    -9,   -17,  -335,  -335,  -335,
    -335,  -335,  -197,  -335,  -203,    37,   -64,  -205,   -32,  -335,
    -208,  -335,  -335,   196,    28,  -239,    43,  -289,  -335,  -335,
    -233,    98,  -335,  -335,  -335,   106,   -30,    12,  -335,  -335,
    -335,  -335,  -335,   -65,  -335,  -335,  -335,  -335,  -335,  -335,
    -335,   -16,  -335,   -38,  -335,  -335,  -335,  -335,  -335,  -335,
    -335,   -52,  -335,  -335,  -335,  -335,   227,    93,  -335,    41,
    -335,  -335,  -335,  -335,  -121,    -5,   -10,  -335,   284,  -108,
    -335,   240,  -180,  -335,   170,  -247,   116,  -100,  -335,  -335,
    -335,  -335
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -145
static const yytype_int16 yytable[] =
{
       8,   103,   180,   121,   178,   300,   306,   344,    18,    54,
     347,   188,    23,    48,   182,   269,   284,   316,   206,    19,
     379,     3,    34,   287,    39,    14,   264,   366,    51,    23,
      71,    74,    62,    18,   302,     4,    31,   304,    48,   240,
       9,    75,    63,   241,    59,    72,     7,   242,     7,   243,
     367,    13,    42,    81,    34,    77,   110,    78,    99,   100,
     101,   351,   379,   225,   105,   322,   230,    50,   308,    23,
     234,   248,   256,    18,   258,   117,    18,     7,    48,   323,
      16,   324,    11,    12,   111,   393,    13,   118,    48,    48,
     253,   126,    50,    81,    18,   179,   190,    48,   137,    20,
      48,    22,    26,    81,    81,    59,   183,   213,   214,   215,
      28,   406,    81,    30,    32,    51,    34,   284,   136,    37,
      18,    18,   241,   385,   228,    33,   242,    57,   243,   137,
       7,   235,    50,     7,    40,    58,    13,    42,    43,    73,
     300,    97,    50,    50,   309,   229,   265,   178,    98,   136,
     193,    50,   102,   104,    50,   107,   -24,   108,   116,     7,
     194,   119,    13,    42,    43,    48,   120,   291,   291,     7,
     195,   124,    13,    42,    43,   297,   303,   241,  -144,  -144,
      81,   242,    18,   243,    80,   125,   207,   184,    18,    18,
      18,    18,   176,   111,   208,   185,   186,   189,   192,    59,
      44,   257,    45,  -144,   231,   232,   209,   271,   272,   273,
     274,   275,   238,   244,  -144,   239,   353,   245,   247,    50,
     210,   249,   211,   266,   330,   138,    44,   212,    45,   251,
      48,   342,   252,   259,   137,   260,   213,   214,   215,   305,
     261,   137,   286,   263,   270,    81,   288,   289,   311,   295,
     296,   357,   299,   310,   358,   312,   138,    18,   236,   313,
     317,   136,   318,   326,   315,   320,   337,    48,   370,   371,
     338,   339,   341,   348,   354,   291,   349,   359,   363,   360,
     362,   365,    51,   373,    50,   137,   378,   374,   388,   382,
     389,    48,   137,   235,   390,   137,   391,   395,   400,   396,
      41,   403,   408,   413,   387,   335,    81,   398,   412,   355,
      24,    70,   136,    35,   109,   136,   409,   404,   352,   254,
     410,    50,   325,    60,   401,   372,   383,   369,   377,   233,
     235,   326,   336,   137,   350,    48,   301,   384,   294,   343,
     326,    48,   346,   411,    48,    50,   394,   386,   399,   407,
      51,   376,   205,   335,   314,   356,    51,   106,   137,    51,
     191,   138,   255,   307,     0,     0,     0,     0,   138,   137,
     137,     0,     0,   326,     0,     0,     0,    48,   136,     0,
     375,     0,     0,     0,     0,   137,   137,   137,     0,    50,
     136,     0,    51,     0,     0,    50,   137,     0,    50,    18,
       0,     0,     0,     0,   137,   335,   335,   335,     0,   137,
     111,     0,   138,     0,     0,     0,   136,   392,     0,   138,
     345,     0,   138,     0,   335,     0,     0,     0,     0,   335,
     277,    50,   402,   343,   346,     0,     7,     0,     0,    13,
      42,    43,     0,   405,     0,     0,     0,     0,     0,     0,
       0,   392,     0,   207,     0,     0,   405,   236,     0,     7,
     138,   208,    13,    42,    43,     0,     0,    74,     0,     0,
       0,     0,     0,   209,   213,   214,   215,    75,     0,   278,
     279,   280,   281,   282,   283,   138,    76,   210,   321,   211,
       0,    77,     0,    78,   212,     0,   138,   138,     0,     0,
       0,     0,     0,   213,   214,   215,     0,     0,    79,     0,
      80,     0,   138,   138,   138,     7,     0,     0,    13,    42,
      43,     0,     0,   138,     0,     0,    44,     0,    45,     0,
       0,   138,   207,     0,     7,     0,   138,    13,    42,    43,
     208,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   207,   209,     0,     0,     0,     0,     0,     0,   208,
       0,     0,     0,     0,     0,     0,   210,     7,   211,     0,
      13,   209,     0,   212,     0,     0,   102,   127,     0,     0,
       0,     0,   213,   214,   215,   210,   128,   211,   129,   130,
       0,     0,   212,     0,     7,   131,   132,    13,     0,     0,
     133,     0,     0,   102,   127,     0,     0,     0,   134,   135,
       7,     0,     0,   331,     0,   129,   332,     0,     0,   102,
     127,     0,   131,   132,     0,     0,     0,   133,     0,   128,
       0,   129,   130,     0,     0,   333,   334,     7,   131,   132,
       0,     0,     0,   133,     0,     0,   102,   127,     0,     0,
       0,   134,   135,     0,     0,     0,   331,     0,   129,   332,
       0,     0,     0,     0,     0,   131,   132,     0,     0,     0,
     133,     0,     0,     0,     0,     0,     0,     0,   333,   334
};

static const yytype_int16 yycheck[] =
{
       4,    66,   107,    97,   104,   238,   245,   296,    12,    32,
     299,   116,    16,    32,   108,   212,   224,   264,   126,    12,
     354,     0,    26,   226,    28,     9,   206,    19,    32,    33,
      49,    14,    26,    37,   239,    37,    20,   242,    57,    51,
      30,    24,    36,    55,    37,    49,     6,    59,     6,    61,
      42,     9,    10,    57,    58,    38,    16,    40,    62,    63,
      65,   308,   396,   127,    69,   270,   130,    32,   248,    73,
     134,   179,   193,    77,   195,    79,    80,     6,    97,   276,
      17,   284,    56,    57,    77,   374,     9,    80,   107,   108,
     184,   101,    57,    97,    98,   105,   119,   116,   102,    53,
     119,    56,    44,   107,   108,    98,   110,    73,    74,    75,
      53,   400,   116,    58,    78,   119,   120,   325,   102,    46,
     124,   125,    55,   362,   128,    56,    59,    78,    61,   133,
       6,   135,    97,     6,    56,    56,     9,    10,    11,    56,
     373,    52,   107,   108,   249,   129,   210,   247,    56,   133,
      26,   116,    15,    56,   119,    59,    54,    32,    32,     6,
      36,    54,     9,    10,    11,   184,    56,   231,   232,     6,
      46,    57,     9,    10,    11,    53,   240,    55,    20,    21,
     184,    59,   186,    61,    57,    57,    23,    52,   192,   193,
     194,   195,    61,   186,    31,    21,    56,    58,    56,   192,
      73,   194,    75,    45,    57,    57,    43,    66,    67,    68,
      69,    70,    52,    21,    56,    57,   310,    56,    56,   184,
      57,    53,    59,    60,   288,   102,    73,    64,    75,    60,
     249,   295,    52,    52,   238,    58,    73,    74,    75,   243,
      56,   245,    32,    58,    57,   249,    51,    41,   252,    45,
      18,   316,    18,    32,   318,   259,   133,   261,   135,    52,
      58,   245,    53,   286,    56,    60,    41,   286,   332,   333,
      58,    53,    58,    58,    32,   339,    60,    58,    56,    53,
      52,    21,   286,    52,   249,   289,   351,    20,    51,    56,
      41,   310,   296,   297,    18,   299,    18,    52,    18,    56,
      31,    57,    20,    18,   368,   289,   310,   384,    58,   313,
      16,    39,   296,    26,    76,   299,   403,   396,   309,   186,
     404,   286,   285,    37,   388,   334,   358,   331,   345,   133,
     334,   354,   289,   337,   306,   354,   238,   360,   232,   296,
     363,   360,   299,   407,   363,   310,   376,   363,   386,   401,
     354,   339,   125,   337,   261,   314,   360,    73,   362,   363,
     120,   238,   192,   247,    -1,    -1,    -1,    -1,   245,   373,
     374,    -1,    -1,   396,    -1,    -1,    -1,   396,   362,    -1,
     337,    -1,    -1,    -1,    -1,   389,   390,   391,    -1,   354,
     374,    -1,   396,    -1,    -1,   360,   400,    -1,   363,   403,
      -1,    -1,    -1,    -1,   408,   389,   390,   391,    -1,   413,
     403,    -1,   289,    -1,    -1,    -1,   400,   374,    -1,   296,
     297,    -1,   299,    -1,   408,    -1,    -1,    -1,    -1,   413,
      29,   396,   389,   390,   391,    -1,     6,    -1,    -1,     9,
      10,    11,    -1,   400,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   408,    -1,    23,    -1,    -1,   413,   334,    -1,     6,
     337,    31,     9,    10,    11,    -1,    -1,    14,    -1,    -1,
      -1,    -1,    -1,    43,    73,    74,    75,    24,    -1,    78,
      79,    80,    81,    82,    83,   362,    33,    57,    58,    59,
      -1,    38,    -1,    40,    64,    -1,   373,   374,    -1,    -1,
      -1,    -1,    -1,    73,    74,    75,    -1,    -1,    55,    -1,
      57,    -1,   389,   390,   391,     6,    -1,    -1,     9,    10,
      11,    -1,    -1,   400,    -1,    -1,    73,    -1,    75,    -1,
      -1,   408,    23,    -1,     6,    -1,   413,     9,    10,    11,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    43,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    57,     6,    59,    -1,
       9,    43,    -1,    64,    -1,    -1,    15,    16,    -1,    -1,
      -1,    -1,    73,    74,    75,    57,    25,    59,    27,    28,
      -1,    -1,    64,    -1,     6,    34,    35,     9,    -1,    -1,
      39,    -1,    -1,    15,    16,    -1,    -1,    -1,    47,    48,
       6,    -1,    -1,    25,    -1,    27,    28,    -1,    -1,    15,
      16,    -1,    34,    35,    -1,    -1,    -1,    39,    -1,    25,
      -1,    27,    28,    -1,    -1,    47,    48,     6,    34,    35,
      -1,    -1,    -1,    39,    -1,    -1,    15,    16,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    25,    -1,    27,    28,
      -1,    -1,    -1,    -1,    -1,    34,    35,    -1,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,    48
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    88,    89,     0,    37,   217,   218,     6,    97,    30,
     202,    56,    57,     9,    90,    91,    17,   203,    97,   109,
      53,    92,    56,    97,   104,   204,    44,   206,    53,   110,
      58,    90,    78,    56,    97,   106,   207,    46,   209,    97,
      56,    92,    10,    11,    73,    75,    90,    93,    94,    95,
      96,    97,    99,   100,   101,   104,   205,    78,    56,   109,
     130,   210,    26,    36,   191,   192,   212,   214,   215,   216,
     110,    94,    97,    56,    14,    24,    33,    38,    40,    55,
      57,    97,   101,   105,   107,   108,   111,   112,   113,   114,
     115,   118,   127,   128,   129,   106,   208,    52,    56,    97,
      97,   202,    15,   170,    56,   202,   205,    59,    32,   114,
      16,   109,   119,   120,   122,   123,    32,    97,   109,    54,
      56,   105,   130,   211,    57,    57,   203,    16,    25,    27,
      28,    34,    35,    39,    47,    48,    90,    97,   131,   134,
     135,   136,   137,   138,   150,   152,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   175,   176,   177,   181,   182,   183,
     184,   185,   186,   187,   189,   190,    61,   213,   214,   203,
     107,   116,   105,    97,    52,    21,    56,   121,   107,    58,
     101,   208,    56,    26,    36,    46,   109,   193,   194,   195,
     197,   198,   199,   200,   201,   193,   206,    23,    31,    43,
      57,    59,    64,    73,    74,    75,    94,    96,    97,    98,
     131,   139,   140,   141,   142,   143,   147,   149,    97,    90,
     143,    57,    57,   150,   143,    97,   131,   132,    52,    57,
      51,    55,    59,    61,    21,    56,   151,    56,   206,    53,
     117,    60,    52,   105,   120,   211,   201,   109,   201,    52,
      58,    56,   196,    58,   209,   143,    60,   143,   144,   139,
      57,    66,    67,    68,    69,    70,   146,    29,    78,    79,
      80,    81,    82,    83,   147,   148,    32,   141,    51,    41,
     174,   143,   162,   164,   162,    45,    18,    53,   133,    18,
     157,   158,   144,   143,   144,    97,   152,   213,   209,   107,
      32,    97,    97,    52,   194,    56,   212,    58,    53,   145,
      60,    58,   144,   139,   141,   142,   101,   102,   178,   179,
     143,    25,    28,    47,    48,    90,   153,    41,    58,    53,
     163,    58,   143,   153,   154,   131,   153,   154,    58,    60,
     151,   212,   117,   105,    32,    97,   196,   170,   143,    58,
      53,   103,    52,    56,   180,    21,    19,    42,   188,    97,
     143,   143,   132,    52,    20,   153,   164,   133,   170,   102,
     124,   125,    56,   145,   101,   152,   178,   143,    51,    41,
      18,    18,   153,   154,   163,    52,    56,   126,   103,   180,
      18,   143,   153,    57,   124,   153,   154,   188,    20,   119,
     126,   143,    58,    18
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 1063 "pascal.y"
    { 
      /* Create a new symbol table. */
      stab = symtab_new ();

      /* Start a new activation record. */
      ar_enter ();

      /* Allocate memory for instructions array */
      instructions = (Instruction *) calloc (MAX_INSTRUCTIONS, sizeof
      (Instruction));
      
      int i;
      
      /* Establish basic types. */
      char *type_names[]  = {"boolean", "integer", "real", "char", "string"};
      TypeID types[] = {TYPE_BOOLEAN, TYPE_INTEGER, TYPE_REAL, TYPE_CHAR,
TYPE_STRING};

      /* Put all basic types in the symbol table. */
      for (i = 0; i < 5; i++)
      {
        AttributeSet *type_attributes = new_attribute_set (1);
      	Type *type = new_type (types[i]);       
      	set_p_attribute (type_attributes, "type", type);
        symtab_put (stab, type_names[i], type_attributes);
      } 

      /* An array to store procedure/function parameters. */
    ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 1093 "pascal.y"
    { 
      tree = (yyvsp[(2) - (2)]); 
      generate_instruction (EXIT, new_iconstant (0), NULL, NULL);
    ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 1107 "pascal.y"
    {
      /* Set the type of the integer constant. */
      AttributeSet *attributes = new_attribute_set (3);
      Type *type = new_type (TYPE_INTEGER);
      set_p_attribute (attributes, "type", type);
      int integer_value = atoi (yytext);
      set_i_attribute (attributes, "ivalue", integer_value);

      /* Store the address of the integer constant. */
      set_p_attribute (attributes, "address", new_iconstant (integer_value));
      
      Node *node = new_tnode (_INTEGER, attributes);
      (yyval) = node;
    ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 1125 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_unsigned_integer_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 1133 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 1135 "pascal.y"
    {
      (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 1142 "pascal.y"
    {
      /* Store the type and value of the real constant in the parse tree. */
      AttributeSet *attributes = new_attribute_set (3);
      Type *type = new_type (TYPE_REAL);
      set_p_attribute (attributes, "type", type);
      float real_value = atof (yytext);
      set_r_attribute (attributes, "rvalue", real_value);

      /* Store the address of the real constant. */
      set_p_attribute (attributes, "address", new_fconstant (real_value));

      Node *node = new_tnode (_REAL, attributes);
      (yyval) = node;
    ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 1165 "pascal.y"
    { (yyval) = new_tnode (_PLUS, new_attribute_set (0)); ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 1167 "pascal.y"
    { (yyval) = new_tnode (_DASH, new_attribute_set (0)); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 1172 "pascal.y"
    {
      /* Set the type of the string constant. */
      AttributeSet *attributes = new_attribute_set (3);
      Type *type = new_type (TYPE_STRING);
      set_p_attribute (attributes, "type", type);
      char *string_value = strdup (yytext);
      set_s_attribute (attributes, "svalue", string_value);

      /* Store the address of the string constant. */
      set_p_attribute (attributes, "address", new_sconstant (string_value));
      
      Node *node = new_tnode (_STRING, attributes);
      (yyval) = node;
    ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 1192 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_s_attribute (attributes, "name", strdup (yytext));
      Node *node = new_tnode (_IDENTIFIER, attributes);
      (yyval) = node;
    ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 1209 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 1211 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 1213 "pascal.y"
    { (yyval) = new_tnode (_NIL, new_attribute_set (0)); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 1215 "pascal.y"
    { (yyval) = new_tnode (_TRUE, new_attribute_set (0)); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 1217 "pascal.y"
    { (yyval) = new_tnode (_FALSE, new_attribute_set (0)); ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 1222 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_signed_number, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 1230 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_signed_identifier, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 1238 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 1240 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 1242 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 1244 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 1246 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 1251 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_constant_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 1259 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 1261 "pascal.y"
    {
      (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 1268 "pascal.y"
    {
      /* Put the new constant in the symbol table. */
      AttributeSet *attributes = new_attribute_set (1);
   
      /* Generate code for constant definition. */
      StacParameter *address = translate_expr (_EQ, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      set_p_attribute (attributes, "address", address);
      
      (yyval) = new_interior_node (_constant_definition, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 1284 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 1286 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 1288 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 1293 "pascal.y"
    {
      /* Get the name of the id. */
      char *id_name = get_s_attribute ((yyvsp[(1) - (3)])->attributes, "name");

      /* Put the new type in the symbol table. */
      TypeID alias_type = get_type_id ((yyvsp[(3) - (3)]));
      Type *type = new_type (alias_type);
      AttributeSet *type_attributes = new_attribute_set (1);
      set_p_attribute (type_attributes, "type", type);
      symtab_put (stab, id_name, type_attributes);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_type_definition, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 1313 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 1315 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 1317 "pascal.y"
    { 
      /* Get the name of the id (new type) from its attributes. */
      char *id_name = get_s_attribute ((yyvsp[(1) - (1)])->attributes, "name");

      /* Create a new type struct. */
      AttributeSet *id_attributes = symtab_get (stab, id_name);
      Type *type = get_p_attribute (id_attributes, "type");
      if (type == NULL)
        fprintf (stderr, "Not a valid type name.\n");

      AttributeSet *attributes = new_attribute_set (1);
      set_p_attribute (attributes, "type", type);
      (yyval) = new_interior_node (_simple_type, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 1337 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_scalar_type, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 1345 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_idlist, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 1353 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 1355 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 1364 "pascal.y"
    {
      /* Create a type for the subrange type. */
      struct SubrangeType *subrange_struct = 
                                   malloc (sizeof (struct SubrangeType));

      
      /* If bounds are incompatible, then throw an appropriate error. */
      if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
        fprintf (stderr, "Incompatible types in subrange!\n");

      printf ("in subrange\n");
      /* Types are compatible, so assign the type to subrange. */
      Type *const_type = get_p_attribute ((yyvsp[(1) - (3)])->attributes, "type");
      subrange_struct->type = const_type;

      /* Retrieve the constant values based on the type. */
      Attribute lower;
      Attribute upper;

      switch (const_type->type_id)
	{
	case (TYPE_INTEGER):
	  lower.ival = get_i_attribute ((yyvsp[(1) - (3)])->attributes, "ivalue");
	  upper.ival = get_i_attribute ((yyvsp[(3) - (3)])->attributes, "ivalue");
	  break;
	case (TYPE_REAL):
	  lower.dval = get_r_attribute ((yyvsp[(1) - (3)])->attributes, "rvalue");
	  upper.dval = get_r_attribute ((yyvsp[(3) - (3)])->attributes, "rvalue");
	  break;
	case (TYPE_STRING):
	  lower.sval = get_s_attribute ((yyvsp[(1) - (3)])->attributes, "svalue");
	  upper.sval = get_s_attribute ((yyvsp[(3) - (3)])->attributes, "svalue");
	  break;
	default:
	  fprintf (stderr, "Constant has the wrong type!\n");
	}
       
      subrange_struct->lower = lower;
      subrange_struct->upper = upper;

      /* Store the subrange attributes with the node. */
      AttributeSet *subrange_attributes = new_attribute_set (1);
      Type *type = new_type (TYPE_SUBRANGE);
      type->info.subrange = subrange_struct;
      set_p_attribute (subrange_attributes, "type", type);
      (yyval) = new_interior_node (_subrange_type, subrange_attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 1417 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 1419 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 1424 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_packed_structured_type, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 1432 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1434 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1436 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 1438 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 1445 "pascal.y"
    {
      int i;

      /* Create a type for the array type. */
      struct ArrayType *array_struct = malloc (sizeof (struct ArrayType));

      /* Get the component type. */
      array_struct->component_type = get_p_attribute ((yyvsp[(6) - (6)])->attributes, "type");

      /* In this implementation, all arrays are one-dimensional. */
      array_struct->dimensions = 1;

      /* Determine how many types to process. */
      int num_types = get_arity ((yyvsp[(3) - (6)]));

      /* Allocate space for an array of index types. */
      array_struct->index_types = malloc (sizeof (struct Type) * num_types);

      /* Process simple_type_list. */
      for (i = 0; i < num_types; i++)
      {
        /* Get the type from type_list and store it in the array. */
        Node *type_node = get_child ((yyvsp[(3) - (6)]), i);
        Type *type = get_p_attribute (type_node->attributes,"type");
        array_struct->index_types[i] = type;
      }

      /* Store the array attributes with the node. */
      AttributeSet *array_attributes = new_attribute_set (1);
      Type *type = new_type (TYPE_ARRAY);
      type->info.array = array_struct;
      set_p_attribute (array_attributes, "type", type);
      (yyval) = new_interior_node (_array_type, array_attributes, 2, (yyvsp[(3) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 1483 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_simple_type_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 1491 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 1493 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 1500 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_record_type, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 1521 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = (yyvsp[(1) - (1)]);
      (yyval)->attributes = attributes;
      Node *fields = get_child ((yyval), 0);
      set_child ((yyval), 0, list2node (_fixed_part, NULL, fields));
    ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 1532 "pascal.y"
    {
      (yyval) = (yyvsp[(2) - (2)]);
      set_child ((yyval), 0, cons ((yyvsp[(1) - (2)]), get_child ((yyval), 0)));
    ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 1537 "pascal.y"
    { 
      (yyval) = new_interior_node (_field_list, NULL, 2, new_epsilon (), (yyvsp[(1) - (1)]));
    ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 1544 "pascal.y"
    { 
      Node *left = new_epsilon ();
      Node *right = new_epsilon ();
      (yyval) = new_interior_node (_field_list, NULL, 2, left, right);
    ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 1550 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 1555 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_record_section, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1563 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant_part, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 1571 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant, attributes, 2, (yyvsp[(1) - (5)]), (yyvsp[(4) - (5)]));
    ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 1576 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1584 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variant_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 1592 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 1594 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 1601 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_set_type, attributes, 1, (yyvsp[(3) - (3)]));
    ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 1611 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_file_type, attributes, 1, (yyvsp[(3) - (3)]));
    ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 1621 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_pointer_type, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 1631 "pascal.y"
    {
      int i;
   
      /* Determine how many ids to process. */
      int num_ids = get_arity ((yyvsp[(1) - (3)]));
      
      /* Put each id with its corresponding type in the symbol table. */
      for (i = 0; i < num_ids; i++)
      {                                                                              
        /* Get the name of the id from its attributes. */
        Node *id = get_child ((yyvsp[(1) - (3)]), i);
        char *id_name = get_s_attribute (id->attributes, "name");
        
        /* Set the type. */
        AttributeSet *sym_attributes = new_attribute_set (2);
        TypeID type_id = get_type_id ((yyvsp[(3) - (3)]));
        Type *type = new_type (type_id);
        set_p_attribute (sym_attributes, "type", type);

	      /* Allocate memory for the variable based on its type 
	       * and assign it a memory location. */
        int offset = ar_alloc (size_of_type (type_id));
	      StacParameter *address = new_relative (BP->info.r, offset);
	      set_p_attribute (sym_attributes, "address", address);

        /* If symbol is already in scope, then throw an error. */
        if (symtab_is_in_scope (stab, id_name))
          fprintf (stderr, "%s is already declared!", id_name);
        else
        {
          symtab_put (stab, id_name, sym_attributes);
        }
      }
      
      /* Associate attributes with the node. */
      AttributeSet *node_attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variable_declaration, node_attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 1673 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 1675 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 1677 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 1682 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variable_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1690 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 1692 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 1699 "pascal.y"
    {
      stop_here ();
      
      /* Retrieve the id attributes from symbol table. */
      char *id_name = get_s_attribute ((yyvsp[(1) - (1)])->attributes, "name");
      AttributeSet *set = symtab_get (stab, id_name);
      if (set == NULL)
      	fprintf (stderr, "%s is not declared!\n", id_name);
      
      /* Set the type. */
      Type *type = get_p_attribute (set, "type");
      AttributeSet *attributes = new_attribute_set (2);
      set_p_attribute (attributes, "type", type);

      /* Retrieve the address of id from the symbol table and store it in the
       * parse tree. */
      StacParameter *address = get_p_attribute (set, "address");
      set_p_attribute (attributes, "address", address);

      (yyval) = new_interior_node (_entire_variable, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 1726 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1728 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 1735 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_indexed_variable, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1745 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_field_designator, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 1760 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_referenced_variable, attributes, 1, (yyvsp[(1) - (2)]));
    ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1770 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1772 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1774 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1776 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1778 "pascal.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1780 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (1);

      /* A little bit of code. */
      StacParameter *address = translate_expr (_NOT, NULL, (yyvsp[(2) - (2)]));
      set_p_attribute (attributes, "address", address);

      (yyval) = new_interior_node (_negate, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 1793 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_set, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1798 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_empty_set, attributes, 0);
    ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1806 "pascal.y"
    {
      /* Get the mulop operator. */
      int operator = get_operator ((yyvsp[(2) - (3)]));

      /* Determine which operation we are performing and thus determine
       * allowed types. If types are inappropriate, throw an appropriate error.
       */
      switch (operator)
      {
       case (_STAR): 
       case (_SLASH):
       case (_DIV):
         if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (get_type_id ((yyvsp[(1) - (3)])) != TYPE_INTEGER && get_type_id ((yyvsp[(1) - (3)])) != TYPE_REAL)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
        break;
       case (_MOD):
         if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (get_type_id ((yyvsp[(1) - (3)])) != TYPE_INTEGER)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
        break;
       case (_AND):
         if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (get_type_id ((yyvsp[(1) - (3)])) != TYPE_BOOLEAN)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
       }

      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (2);
      TypeID type_id = get_type_id ((yyvsp[(3) - (3)]));
      Type *type = new_type (type_id);
      set_p_attribute (attributes, "type", type);

      /* Generate some code. */
      StacParameter *address = translate_expr (operator, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      set_p_attribute (attributes, "address", address);

      /* We do not perform operand type conversion in this implementation, 
       * so just pass the children.
       */
      (yyval) = new_interior_node (_term, attributes, 3, (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1852 "pascal.y"
    {
      (yyval) = (yyvsp[(1) - (1)]);
    ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1859 "pascal.y"
    {
      /* Get the addop operator. */
      int operator = get_operator ((yyvsp[(2) - (3)]));

      /* Determine which addop operation we are performing. If addition or 
       * subtraction, then allowed types are integers and reals.
       * If types are inappropriate, throw an appropriate error.
       */
      if ((operator == _PLUS) || (operator == _DASH))
      {
        if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
          fprintf (stderr, "Incompatible types in expression!\n");
        if (get_type_id ((yyvsp[(1) - (3)])) != TYPE_INTEGER && get_type_id ((yyvsp[(1) - (3)])) != TYPE_REAL)
          fprintf (stderr, "Operands have incorrect type in expression.\n");
      }
      /* Otherwise, the operator is OR. */
      else 
      {
        if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
          fprintf (stderr, "Incompatible types in expression!\n");
        if (get_type_id ((yyvsp[(1) - (3)])) != TYPE_BOOLEAN)
          fprintf (stderr, "Operands have incorrect type in expression.\n");
      } // if
 
      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (2);
      Type *type = new_type (get_type_id ((yyvsp[(3) - (3)])));
      set_p_attribute (attributes, "type", type);
      
      /* Time to generate code. */
      StacParameter *address = translate_expr (operator, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      set_p_attribute (attributes, "address", address);

      /* We do not do operand type conversion in this implementation, 
       * so just pass the children.
       */
      (yyval) = new_interior_node (_simple, attributes, 3, (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1898 "pascal.y"
    {
    int operator = get_operator ((yyvsp[(2) - (2)]));		

    AttributeSet *attributes = new_attribute_set (1);
        
    /* Generate some exquisite code! */
    StacParameter *address = translate_expr (operator, NULL, (yyvsp[(2) - (2)]));
    set_p_attribute (attributes, "address", address);

    (yyval) = new_interior_node (_simple, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1910 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1915 "pascal.y"
    {
      int operator = get_operator ((yyvsp[(2) - (3)]));
	
      /* If types are incompatible or inappropriate, throw an appropriate 
       * error. 
       */
      if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
        fprintf (stderr,"Incompatible types in expression!\n"); 
      if (get_type_id ((yyvsp[(1) - (3)])) != TYPE_INTEGER && get_type_id ((yyvsp[(1) - (3)])) != TYPE_REAL)
        fprintf (stderr, "Operands have incorrect type in expression.\n");

      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (3);
      Type *type = new_type (get_type_id ((yyvsp[(3) - (3)])));
      set_p_attribute (attributes, "type", type);

      /* Code generation! */
      StacParameter *address = translate_expr (operator, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      set_p_attribute (attributes, "address", address);

      /* Save the operator, so that we know if a boolean expression. */
      set_i_attribute (attributes, "operator", operator);

      /* We do not do operand conversion in this implementation, so just pass
       * the children.
       */
      (yyval) = new_interior_node (_expr, attributes, 3, (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1944 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1949 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_expr_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1957 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1959 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1970 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _STAR, attributes);
    ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1975 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _SLASH, attributes);
    ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1980 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _DIV, attributes);
    ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1985 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _MOD, attributes);
    ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1990 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _AND, attributes);
    ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 2000 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _PLUS, attributes);
    ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 2005 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _DASH, attributes);
    ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 2010 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _OR, attributes);
    ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 2020 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _LT, attributes);
    ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 2025 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _LE, attributes);
    ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 2030 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _EQ, attributes);
    ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 2035 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _NE, attributes);
    ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 2040 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _GE, attributes);
    ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 2045 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _GT, attributes);
    ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 2050 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _IN, attributes);
    ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 2064 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_call, attributes, 1, (yyvsp[(1) - (3)]));
    ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 2069 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_call, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 2087 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_statement_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 2095 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 2097 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 2102 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 2104 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 2109 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 2111 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 2116 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 2118 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 2123 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (1);
      Instruction *instruction = get_p_attribute((yyvsp[(3) - (3)])->attributes, "instruction");
      set_p_attribute (attributes, "instruction", instruction);
      (yyval) = new_interior_node (_labeled_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 2133 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_labeled_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 2141 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 2143 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 2148 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 2155 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 2157 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 2159 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 2161 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 2163 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 2175 "pascal.y"
    {
      if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
        fprintf (stderr, "Expression has incompatible type in assignment!\n");
      
      /* Determine which instruction to use. */
      OpCode opcode;
      switch (get_type_id ((yyvsp[(1) - (3)])))
     	{
       	case (TYPE_INTEGER):
       	case (TYPE_BOOLEAN):
       	  opcode = IMOV;
       	  break;
       	case (TYPE_REAL):
       	  opcode = FMOV;
       	  break;
       	case (TYPE_CHAR):
       	  opcode = CMOV;
       	  break;
       	case (TYPE_STRING):
       	  opcode = SMOV;
       	  break;
       	default:
       	  fprintf (stderr, "Assignment cannot be completed!\n");
     	}
     
      /* Retrieve the address of the variable. */
      StacParameter *var_address = get_p_attribute ((yyvsp[(1) - (3)])->attributes, "address");

      /* Retrieve the address of the expression. */
      StacParameter *expr_address = get_p_attribute ((yyvsp[(3) - (3)])->attributes, "address");

      /* Build code for assignment. */
      Instruction *instruction = malloc (sizeof (Instruction));
      build_instruction (instruction, opcode, var_address, expr_address, NULL);

      AttributeSet *attributes = new_attribute_set (1);
      set_p_attribute (attributes, "instruction", instruction);
      (yyval) = new_interior_node (_assignment_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 2220 "pascal.y"
    {
      int num_write_params = get_arity ((yyvsp[(3) - (4)]));

      int i; 
      for (i = 0; i < num_write_params; i++)
      {
        Node *write_param = get_child ((yyvsp[(3) - (4)]), i);

        /* Determine which instruction to use. */
        OpCode opcode;
        switch (get_type_id (write_param))
       	{
         	case (TYPE_INTEGER):
         	case (TYPE_BOOLEAN):
         	  opcode = IWRITE;
         	  break;
         	case (TYPE_REAL):
         	  opcode = FWRITE;
         	  break;
         	case (TYPE_CHAR):
         	  opcode = CWRITE;
         	  break;
         	case (TYPE_STRING):
         	  opcode = SWRITE;
         	  break;
         	default:
    	      fprintf (stderr, "Unknown type of expression to output!\n");
       	} // switch

         StacParameter *param_address = 
                       get_p_attribute (write_param->attributes, "address");

         /* Generate the code! */
         generate_instruction (opcode, param_address, NULL, NULL);
      } // for

      AttributeSet *attributes = new_attribute_set (0);	  
      (yyval) = new_interior_node (_output_statement, attributes, 1, (yyvsp[(3) - (4)]));
    ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 2260 "pascal.y"
    {
      int num_write_params = get_arity ((yyvsp[(3) - (4)]));

      int i; 
      for (i = 0; i < num_write_params; i++)
      {
        Node *write_param = get_child ((yyvsp[(3) - (4)]), i);

        StacParameter *param_address = 
                       get_p_attribute (write_param->attributes, "address");

         /* Generate the code! */
         generate_instruction (WRITELN, param_address, NULL, NULL);
      } // for

      AttributeSet *attributes = new_attribute_set (0);	  
      (yyval) = new_interior_node (_output_statement, attributes, 1, (yyvsp[(3) - (4)]));
    ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 2282 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_write_parameter_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 2290 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 2292 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));  ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 2297 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 2304 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_call, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 2309 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_call, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 2319 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_goto_statement, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 2329 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_empty_statement, attributes, 0);
    ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 2339 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 2341 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 2343 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 2345 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 2350 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 2352 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 2354 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 2361 "pascal.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 2368 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 2370 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 2375 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 2377 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 2384 "pascal.y"
    {
      /* Generate labels for consequent and end. */
      generate_label (CONSEQUENT_LABEL);
      generate_label (END_LABEL);
      
      /* Generate control flow instructions. */
      translate_bexp ((yyvsp[(2) - (2)]), consequent_label, end_label);
      
      generate_instruction (_LABL, consequent_label, NULL, NULL);
     ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 2395 "pascal.y"
    {
      /* Code for the consequent */
      Instruction *instruction = get_p_attribute ((yyvsp[(5) - (5)])->attributes, "instruction");
      //generate_statement (instruction);
      free (instruction);
      generate_instruction (JMP, end_label, NULL, NULL);

      /* We do not forget about the end label. */
      generate_instruction (_LABL, end_label, NULL, NULL);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_statement, attributes, 2, (yyvsp[(2) - (5)]), (yyvsp[(5) - (5)]));
    ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 2412 "pascal.y"
    { 
      /* Generate labels for consequent, alternate, and end. */
      generate_label (CONSEQUENT_LABEL);
      generate_label (ALTERNATE_LABEL);
      generate_label (END_LABEL);

      /* Generate control flow instructions. */
      translate_bexp ((yyvsp[(2) - (6)]), consequent_label, alternate_label);

      generate_instruction (_LABL, consequent_label, NULL, NULL);
  
      /* Code for the consequent */
      // translate statement ($4);
      generate_instruction (JMP, end_label, NULL, NULL);
      
      generate_instruction (_LABL, alternate_label, NULL, NULL);

      /* Code for the alternate. */
      // translate statement ($5);
      generate_instruction (JMP, end_label, NULL, NULL);
      
      /* We do not forget about the end label. */
      generate_instruction (_LABL, end_label, NULL, NULL);
      
      /* Finally, create the node. */
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_else_statement, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 2444 "pascal.y"
    {
      /* Generate labels for consequent, alternate, and end. */
      generate_label (CONSEQUENT_LABEL);
      generate_label (ALTERNATE_LABEL);
      generate_label (END_LABEL);

      /* Generate control flow instructions. */
      translate_bexp ((yyvsp[(2) - (6)]), consequent_label, alternate_label);

      generate_instruction (_LABL, consequent_label, NULL, NULL);

      /* Code for the consequent */
      // translate statement ($4);
      generate_instruction (JMP, end_label, NULL, NULL);
      
      generate_instruction (_LABL, alternate_label, NULL, NULL);

      /* Code for the alternate. */
      // translate statement ($5);
      generate_instruction (JMP, end_label, NULL, NULL);
      
      /* We do not forget about the end label. */
      generate_instruction (_LABL, end_label, NULL, NULL);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_else_statement, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 2478 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_case_statement, attributes, 2, (yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]));
    ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 2486 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_case_element, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 2494 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_case_element_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 2502 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 2504 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 2511 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 2513 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 2515 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 172:

/* Line 1455 of yacc.c  */
#line 2520 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 2522 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 174:

/* Line 1455 of yacc.c  */
#line 2529 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_while_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 2537 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_while_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 2547 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 2557 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_for_statement, attributes, 5, 
                              (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(8) - (8)]));
    ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 2566 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_for_statement, attributes, 5, 
                              (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(8) - (8)]));
    ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 2575 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_direction, _TO, attributes);
    ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 2580 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_direction, _DOWNTO, attributes);
    ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 2590 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 2598 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 2614 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_declaration, attributes, 7,
                              (yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
    ;}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 2623 "pascal.y"
    {
      int i;
      Type *param_type;
      char *param_name;
            
      /* Get the name of the id from its attributes. */ 
      char *id_name = get_s_attribute ((yyvsp[(2) - (6)])->attributes, "name");

      /* Construct the procedure type. */
      int num_params = get_num_params ();
      Type *type = function_procedure_type (NULL, num_params);

      /* Store info about procedure's params. */
      AttributeSet *proc_attributes = new_attribute_set (1);
      set_p_attribute (proc_attributes, "type", type);

      /* Store the size that needs to be allocated for the procedure's ar. */
      /* NEED TO FILL IN! */
      // set_p_attribute (proc_attributes, "frame_size", ...

      /* If procedure is already in scope, then throw an error. */
      if (symtab_is_in_scope (stab, id_name))
        fprintf (stderr, "%s is already declared!", id_name);
      else
      {
        /* Put procedure in the current scope and start a new scope. */
        symtab_put (stab, id_name, proc_attributes);
        if (symtab_enter (stab) == 0)
          fprintf (stderr, "Could not create another scope!\n");

        /* Put formal parameters in the new scope. */
        for (i = 0; i < num_params; i++)
        {
          /* Get the current param attributes. */
       	  param_type = get_param_type (type, i);
       	  param_name = get_param_name (type, i);
       	  AttributeSet *param_attributes = new_attribute_set (1);
          set_p_attribute (param_attributes, "type", param_type);
          symtab_put (stab, param_name, param_attributes);
        }
      }

      /* Clear the array of parameters. */
      clear_params ();

      /* Associate attributes with the node. */
      AttributeSet *node_attributes = new_attribute_set (1);
      set_p_attribute (node_attributes, "type", type);
      (yyval) = new_interior_node (_procedure_heading, node_attributes, 2, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]));
    ;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 2677 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 2679 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 2684 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 2686 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 2688 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 2690 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 2695 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_formals_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 2703 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 2705 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 2710 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_value_parameter, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 2718 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variable_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 2726 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 2734 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 2742 "pascal.y"
    {
      int i, len;

      /* Determine how many ids to process. */
      int num_ids = get_arity ((yyvsp[(1) - (3)]));

      /* Store parameters in a global array params. */
      /* Put each id with its corresponding type in the symbol table. */
      for (i = 0;i < num_ids; i++)
      {
        /* Same as in variable_declaration- make it into a FUNCTION! */
        /* Get the name of the id from its attributes. */
	Node *id = get_child ((yyvsp[(1) - (3)]), i);
	len = strlen (get_s_attribute (id->attributes, "name"));
	char *id_name = get_s_attribute (id->attributes, "name");

        /* Get the type of $3 from symbol table and associate it with id. */
        char *type_name = get_s_attribute ((yyvsp[(3) - (3)])->attributes, "name");
        AttributeSet *set = symtab_get (stab, type_name);
	Type *type = get_p_attribute (set, "type");

        /* Place the name and type into the params array. */
	insert_param (id_name, type);
      }

      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_parameter_group, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 2774 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 2777 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_label_declaration_part, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 2785 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 2788 "pascal.y"
    {  
     (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 2794 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_constant_definition_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 2802 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 2804 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 2808 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 2810 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 2815 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_type_definition_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 2823 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 2825 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 2830 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 2832 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 2837 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variable_declaration_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 2845 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 2847 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 2852 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 2854 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_procedure_declaration_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 2862 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 2864 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 2869 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
#line 2871 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 222:

/* Line 1455 of yacc.c  */
#line 2884 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_declaration, attributes, 7,
                              (yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
    ;}
    break;

  case 223:

/* Line 1455 of yacc.c  */
#line 2893 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_heading, attributes, 3, (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(7) - (8)]));
    ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 2910 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program, attributes, 7,
                              (yyvsp[(1) - (8)]), (yyvsp[(2) - (8)]), (yyvsp[(3) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(7) - (8)]));
    ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 2919 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 0);
    ;}
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 2924 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 227:

/* Line 1455 of yacc.c  */
#line 2929 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 2, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]));
    ;}
    break;



/* Line 1455 of yacc.c  */
#line 5274 "pascal.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 2935 "pascal.y"


//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Dealing with operators and translation
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

/* Copied from pascal.y by Sam Rebelsky. */
/* Retrieve opcode for arithmetic expression. */
OpCode
get_arithmetic_opcode (int operator, TypeID type_id)
{
  if (type_id == TYPE_INTEGER)
    {
      switch (operator)
        {
	case (_PLUS):
	  return IADD;
	case _DASH:
	  return ISUB;
	case _STAR:
	  return IMUL;
	case _SLASH:
	  return IDIV;
	case _DIV:
	  return IDIV;
	case _MOD:
	  return IMOD;
	default:
	  return NOOP;
        }
    }
  else if (type_id == TYPE_REAL)
    {
      switch (operator)
        {
	case _PLUS:
	  return FADD;
	case _DASH:
	  return FSUB;
	case _STAR:
	  return FMUL;
	case _SLASH:
	  return FDIV;
	default:
	  return NOOP;
        }
    }
  else
    {
      return NOOP;
    } // other type
}

/* STUB. */
/* Retrieve opcode for boolean expression. */
OpCode
get_boolean_opcode (int operator, TypeID type_id)
{
  //  OpCode opcode;
  return NOOP;
}

/* Retrieve opcode for assignment expression. */
OpCode
get_assignment_opcode (TypeID type_id)
{
  switch (type_id)
    {
    case TYPE_INTEGER:
      return IMOV;
    case TYPE_REAL:
      return FMOV;
    case TYPE_CHAR:
      return CMOV;
    case TYPE_STRING:
      return SMOV;
    default:
      return NOOP;
    }
}

/* Check if it is an arithmetic operator. */
int
is_arithmetic_operator (int operator)
{
  if ((operator <= _AND) || (ADDOPS_END <= operator))
    return 0;
  else 
    return 1;
}

/* MORE OPERATORS! */
/* Check if it is a boolean operator. */
int
is_boolean_operator (int operator)
{
  if ((operator != _AND) && (operator != _OR))
    return 0;
  else
    return 1;
}

/* Check if it is an assignment operator. */
int
is_assignment_operator (int operator)
{
  if ((operator != _ASSIGN) && (operator != _EQ))
    return 0;
  else 
    return 1;
}

/* Retrieve opcode given an operator. */
OpCode
get_opcode (int operator, TypeID operand_type)
{
  OpCode opcode;

  /* Determine what kind of operation we want. */
  if (is_arithmetic_operator (operator))
    opcode = get_arithmetic_opcode (operator, operand_type);
  else if (is_boolean_operator (operator))
    opcode = get_boolean_opcode (operator, operand_type);
  else if (is_assignment_operator (operator))
    opcode = get_assignment_opcode (operand_type);
  else 
    { /* If no opcode, then signal error. */
      fprintf (stderr, "This operation is invalid.\n");
      return NOOP;
    }
      
  return opcode;
} // get_opcode
  
/* Translate an expression into three-address code. YAY! */
StacParameter *
translate_expr (int operator, Node *left, Node *right)
{
  StacParameter *left_param;
  StacParameter *right_param;
  
  TypeID operand_type = get_type_id (left);
  OpCode opcode = get_opcode (operator, operand_type);

  /* Get the address from left and right operands if they exist. */
  if (left != NULL)
    left_param = get_p_attribute (left->attributes, "address");
  else
    left_param = NULL;
  if (right != NULL)
    right_param = get_p_attribute (right->attributes, "address");
  else
    right_param = NULL;

  /* Generate address for expression. */
  int offset = ar_alloc (size_of_type (operand_type));
  StacParameter *address = new_relative (BP->info.r, offset);
  generate_instruction (opcode, address, left_param, right_param);

  return address;
} // translate_exp


/* Translate a boolean expression. */
void
translate_bexp (Node *node, StacParameter *true_label, StacParameter *false_label)
{
  /* Check if the operator is a boolean operator. */
  int operator = get_i_attribute (node->attributes, "operator");

  Node *loperand_node = get_child (node, 0);
  Node *roperand_node = get_child (node, 2);
  StacParameter *loperand = get_p_attribute (loperand_node->attributes, "address");
  StacParameter *roperand = get_p_attribute (roperand_node->attributes, "address");

  /* An operand can either be an integer or a real.*/
  if (loperand->type == ICONSTANT)
    switch (operator)
      {
      case _LT:
        generate_instruction (JGTI, true_label, roperand, loperand);
      case _LE:
        generate_instruction (JLEI, true_label, loperand, roperand);
      case _EQ:
        generate_instruction (JEQI, true_label, loperand, roperand);
      case _NE:
        generate_instruction (JNEI, true_label, loperand, roperand);
      case _GE:
        generate_instruction (JLEI, true_label, roperand, loperand);
      case _GT:
        generate_instruction (JGTI, true_label, loperand, roperand);
      case _IN:

        /* NOT YET IMPLEMENTED */
      case _AND:
      case _OR:
      case _NOT:
      case _TRUE:
        generate_instruction (JMP, true_label, NULL, NULL);
      case _FALSE:
        generate_instruction (JMP, false_label, NULL, NULL);

      default:
        fprintf (stderr, "Expected a boolean expression in conditional statement!\n");
      } // switch
  else // it must be a real
    switch (operator)
      {
      case _LT:
        generate_instruction (JGTF, true_label, roperand, loperand);
      case _LE:
        generate_instruction (JLEF, true_label, loperand, roperand);
      case _EQ:
        generate_instruction (JEQF, true_label, loperand, roperand);
      case _NE:
        generate_instruction (JNEF, true_label, loperand, roperand);
      case _GE:
        generate_instruction (JLEF, true_label, roperand, loperand);
      case _GT:
        generate_instruction (JGTF, true_label, loperand, roperand);
      case _IN:

        /* NOT YET IMPLEMENTED */
      case _AND:
      case _OR:
      case _NOT:
      case _TRUE:
        generate_instruction (JMP, true_label, NULL, NULL);
      case _FALSE:
        generate_instruction (JMP, false_label, NULL, NULL);

      default:
        fprintf (stderr, "Expected a boolean expression in conditional statement!\n");
      } // switch
} // translate_bexp


/* Our beautiful lexer. */
#include "lex.yy.c"

/* Code for handling attributes. */
#include "attribute.c"

/* Code for building parse trees. */
#include "parse-tree.c"

