
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
    _bexpr,
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
    "bexpr",
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
  generate_instruction (instruction->op, instruction->params[0], instruction->params[1], instruction->params[2]);
}

/* Generate an instruction for all instructions in instructions array. */
void
generate_instructions (Instruction *instructions[])
{
  /* Get the first instruction. */
  Instruction *instruction = instructions[0];

  /* Process all instructions in the instructions array. */
  while (instruction != NULL)
    generate_statement (instruction);
}

/* Generate instructions for all constituents of procedure and function. */
void
generate_procedure_function (Node *procedure_function_children[])
{
  int i;
  
  /* Generate instructions procedure's or function's children. */
  for (i = 0; i < 7; i++)
    {
      /* Retrieve the instructions array and pass it on. */
      Instruction **instructions; // NOT IMPLEMENTED YET
      generate_instructions (instructions);
    }
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Labels
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

int num_labels = 0;

#define LABEL_LENGTH 20

typedef enum Label
  {
    CONSEQUENT_LABEL,
    ALTERNATE_LABEL,
    END_LABEL
  } Label;

StacParameter *consequent_label;
StacParameter *alternate_label;
StacParameter *end_label;

/* VERY SUBOPTIMAL: REMEMBER TO IMPROVE DESIGN OF LABEL */
/* Generate next unique label. */
void
generate_label (Label label_type)
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
void translate_bexpr (Node *node, StacParameter *truelabel, StacParameter *falselabel);

/* STUB */
void
init_symtab ()
{
}

/* Declare the symbol table for the program. */
SymTab *stab;


/* Line 189 of yacc.c  */
#line 1045 "pascal.tab.c"

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
#line 1173 "pascal.tab.c"

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
#define YYLAST   618

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  87
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  131
/* YYNRULES -- Number of rules.  */
#define YYNRULES  226
/* YYNRULES -- Number of states.  */
#define YYNSTATES  411

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
     415,   420,   427,   434,   440,   444,   447,   448,   452,   454,
     456,   458,   460,   462,   467,   472,   477,   486,   495,   497,
     499,   504,   509,   517,   524,   525,   527,   529,   531,   533,
     535,   538,   539,   543,   545,   548,   551,   554,   558,   559,
     563,   564,   567,   571,   572,   576,   577,   580,   584,   585,
     589,   590,   593,   597,   598,   602,   603,   607,   608,   612,
     614,   616,   624,   633,   642,   643,   647
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      88,     0,    -1,    -1,    89,   216,    -1,     9,    -1,    90,
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
      -1,   171,    -1,   180,    -1,   188,    -1,   172,    -1,   181,
      -1,   189,    -1,    15,   150,    21,    -1,   174,    -1,   176,
      -1,   173,    -1,   175,    -1,    28,   143,    41,   153,    -1,
      28,   143,    41,   153,    20,   153,    -1,    28,   143,    41,
     153,    20,   154,    -1,    16,   143,    32,   178,    21,    -1,
     102,    52,   152,    -1,   177,   179,    -1,    -1,    56,   177,
     179,    -1,   182,    -1,   184,    -1,   185,    -1,   183,    -1,
     186,    -1,    47,   143,    18,   153,    -1,    47,   143,    18,
     154,    -1,    39,   150,    45,   143,    -1,    25,    97,    51,
     143,   187,   143,    18,   153,    -1,    25,    97,    51,   143,
     187,   143,    18,   154,    -1,    42,    -1,    19,    -1,    48,
     132,    18,   153,    -1,    48,   132,    18,   154,    -1,   191,
     201,   202,   205,   208,   211,   170,    -1,    36,    97,    57,
     192,    58,    56,    -1,    -1,   194,    -1,   196,    -1,   197,
      -1,   198,    -1,   199,    -1,   193,   195,    -1,    -1,    56,
     193,   195,    -1,   200,    -1,    46,   200,    -1,    26,   200,
      -1,    36,   109,    -1,   109,    52,    97,    -1,    -1,    30,
      91,    56,    -1,    -1,    17,   203,    -1,   104,    56,   204,
      -1,    -1,   104,    56,   204,    -1,    -1,    44,   206,    -1,
     106,    56,   207,    -1,    -1,   106,    56,   207,    -1,    -1,
      46,   209,    -1,   130,    56,   210,    -1,    -1,   130,    56,
     210,    -1,    -1,   213,    56,   212,    -1,    -1,   213,    56,
     212,    -1,   190,    -1,   214,    -1,   215,   201,   202,   205,
     208,   211,   170,    -1,    26,    97,    57,   192,    58,    52,
      97,    56,    -1,   217,   201,   202,   205,   208,   211,   170,
      61,    -1,    -1,    37,    97,    56,    -1,    37,    97,    57,
     109,    58,    56,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1091,  1091,  1091,  1134,  1152,  1161,  1162,  1169,  1187,
    1188,  1192,  1194,  1199,  1219,  1236,  1238,  1240,  1242,  1244,
    1249,  1257,  1265,  1267,  1269,  1271,  1273,  1278,  1287,  1288,
    1295,  1311,  1313,  1315,  1320,  1340,  1342,  1344,  1364,  1372,
    1381,  1382,  1391,  1444,  1446,  1451,  1459,  1461,  1463,  1465,
    1472,  1510,  1519,  1520,  1527,  1548,  1559,  1564,  1572,  1577,
    1582,  1590,  1598,  1603,  1611,  1620,  1621,  1628,  1638,  1648,
    1658,  1700,  1702,  1704,  1709,  1718,  1719,  1726,  1753,  1755,
    1762,  1772,  1787,  1797,  1799,  1801,  1803,  1805,  1807,  1820,
    1825,  1833,  1879,  1886,  1925,  1937,  1942,  1971,  2027,  2036,
    2037,  2048,  2053,  2058,  2063,  2068,  2078,  2083,  2088,  2098,
    2103,  2108,  2113,  2118,  2123,  2128,  2142,  2147,  2165,  2174,
    2175,  2180,  2182,  2187,  2189,  2194,  2196,  2201,  2211,  2219,
    2221,  2226,  2233,  2235,  2237,  2239,  2241,  2253,  2298,  2338,
    2360,  2369,  2370,  2375,  2382,  2387,  2397,  2408,  2417,  2419,
    2421,  2423,  2428,  2430,  2432,  2439,  2446,  2448,  2453,  2455,
    2462,  2485,  2517,  2551,  2559,  2567,  2576,  2577,  2584,  2586,
    2588,  2593,  2595,  2602,  2610,  2620,  2630,  2639,  2648,  2653,
    2663,  2671,  2681,  2702,  2757,  2758,  2763,  2765,  2767,  2769,
    2774,  2783,  2784,  2789,  2797,  2805,  2813,  2821,  2854,  2856,
    2865,  2867,  2873,  2882,  2883,  2888,  2889,  2894,  2903,  2904,
    2910,  2911,  2916,  2925,  2926,  2932,  2933,  2942,  2943,  2948,
    2950,  2957,  2978,  2988,  3005,  3009,  3014
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
  "unsafe_conditional_statement", "if_then_statement",
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
     173,   174,   175,   176,   177,   178,   179,   179,   180,   180,
     180,   181,   181,   182,   183,   184,   185,   186,   187,   187,
     188,   189,   190,   191,   192,   192,   193,   193,   193,   193,
     194,   195,   195,   196,   197,   198,   199,   200,   201,   201,
     202,   202,   203,   204,   204,   205,   205,   206,   207,   207,
     208,   208,   209,   210,   210,   211,   211,   212,   212,   213,
     213,   214,   215,   216,   217,   217,   217
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
       4,     6,     6,     5,     3,     2,     0,     3,     1,     1,
       1,     1,     1,     4,     4,     4,     8,     8,     1,     1,
       4,     4,     7,     6,     0,     1,     1,     1,     1,     1,
       2,     0,     3,     1,     2,     2,     2,     3,     0,     3,
       0,     2,     3,     0,     3,     0,     2,     3,     0,     3,
       0,     2,     3,     0,     3,     0,     3,     0,     3,     1,
       1,     7,     8,     8,     0,     3,     6
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,   224,     1,     0,     3,   198,    14,     0,     0,
     200,   225,     0,     4,     6,     0,     0,   205,    40,     0,
       0,     5,   199,     0,     0,   201,     0,   210,     0,    39,
       0,     6,     0,   203,     0,     0,   206,     0,   215,    40,
     226,     7,     8,    13,    12,    11,     9,    10,    22,     0,
      26,    24,    23,    25,    30,     0,   202,     0,   208,     0,
       0,   211,     0,     0,   219,   198,     0,     0,   220,   198,
      41,    20,    21,   203,     0,     0,     0,     0,     0,     0,
       0,    37,     0,    34,    31,    35,    36,    32,    44,    43,
      46,    47,    48,    49,    33,     0,   207,     0,   213,     0,
       0,   200,   147,     0,   217,   200,   204,     0,     0,    45,
       0,     0,     0,    55,    58,    57,     0,    69,     0,     0,
     208,    70,     0,   212,   184,   184,   205,     0,     0,     0,
       0,     0,     0,   147,     0,     0,     0,    77,     0,    71,
      72,    78,    79,    73,     0,   119,   121,   122,   123,   125,
     124,   126,   129,   132,   133,   134,   135,   136,   130,   131,
     148,   149,   152,   158,   156,   159,   157,   150,   153,   168,
     171,   169,   170,   172,   151,   154,   223,   216,     0,   205,
      52,     0,    68,     0,     0,    54,     0,    56,    67,    38,
      42,   209,   213,     0,     0,     0,     0,     0,   191,   185,
     186,   187,   188,   189,   193,     0,   210,    19,    17,    18,
       0,     0,     0,   107,   108,   106,    15,    16,    77,    84,
      83,    92,    86,    95,    97,     0,     0,    85,     0,   146,
       0,     0,     0,     0,     0,    77,    75,     0,   147,     0,
       0,    82,     0,     0,   155,   147,   118,   217,   210,     0,
      51,     0,     0,    60,    59,   214,   195,   196,   194,     0,
       0,     0,   190,     0,   215,     0,    90,    99,     0,    88,
       0,   105,   103,   104,   102,   101,     0,   115,   111,   113,
     114,   110,   109,   112,     0,     0,     0,    94,     0,   147,
     143,     0,   141,     0,     0,   147,     0,    74,   147,   127,
     128,     0,   137,     0,    81,   119,   218,   215,    52,     0,
       0,   197,     0,   191,   183,     0,    87,     0,    98,    89,
     116,     0,    91,    93,    96,    28,     0,   166,     0,     0,
       0,     0,     0,     0,     0,   160,   138,     0,   140,   139,
     175,   173,   174,    75,   180,   181,   145,    80,   120,     0,
      53,    50,     0,     0,   192,   182,    99,   117,     0,    27,
     147,     0,   165,   163,   179,   178,     0,     0,     0,     0,
       0,   147,   147,   141,    76,   221,    63,    65,    61,   222,
     100,    28,   164,   166,     0,     0,   147,   147,   147,   161,
     162,   142,     0,     0,    64,    29,   167,   147,     0,     0,
       0,    65,   176,   177,     0,   147,     0,    66,     0,    62,
     147
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    46,    15,    21,    47,   216,    49,   217,
     218,   219,    52,    53,    82,   326,   359,    55,    83,    95,
      84,    85,   196,    29,    86,    87,    88,    89,    90,   181,
     250,    91,   112,   113,   187,   114,   115,   377,   378,   394,
      92,    93,    94,   122,   220,   237,   297,   139,   140,   141,
     142,   143,   221,   222,   223,   224,   267,   268,   318,   276,
     226,   285,   227,   144,   246,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,   291,   338,   292,   155,   156,
     157,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     327,   328,   362,   167,   168,   169,   170,   171,   172,   173,
     366,   174,   175,    64,    65,   197,   198,   199,   262,   200,
     201,   202,   203,   204,    10,    17,    25,    56,    27,    36,
      96,    38,    61,   123,    66,   177,    67,    68,    69,     5,
       6
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -337
static const yytype_int16 yypact[] =
{
    -337,    48,    35,  -337,    89,  -337,    51,  -337,    29,    87,
      82,  -337,    89,  -337,    58,    67,    89,    81,    83,    76,
      87,  -337,  -337,    64,    88,  -337,    89,   107,    89,  -337,
      93,    58,   129,    89,    79,   104,  -337,    89,     1,    83,
    -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,    46,
    -337,  -337,  -337,  -337,  -337,   111,  -337,   255,    89,   119,
     117,  -337,    89,    89,  -337,    51,   159,   125,  -337,    51,
    -337,  -337,  -337,    89,   126,   144,   103,    34,   154,    89,
      89,   140,   142,  -337,  -337,  -337,  -337,  -337,  -337,  -337,
    -337,  -337,  -337,  -337,  -337,   147,  -337,   255,    89,   149,
     150,    82,   519,   136,     1,    82,  -337,    57,   255,  -337,
      89,   170,   193,  -337,   168,  -337,    57,  -337,   174,   129,
      89,  -337,   172,  -337,   120,   120,    81,   465,    89,    87,
     465,   179,   181,   519,   465,    89,   188,    62,   162,  -337,
    -337,  -337,  -337,  -337,   212,   186,  -337,  -337,  -337,  -337,
    -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,
    -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,
    -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,   190,    81,
     191,   187,  -337,   197,   255,  -337,    34,  -337,  -337,  -337,
    -337,  -337,    89,    89,    89,    89,   199,   194,   198,  -337,
    -337,  -337,  -337,  -337,  -337,   195,   107,  -337,  -337,  -337,
     465,   152,   141,  -337,  -337,  -337,  -337,  -337,   201,  -337,
     -12,  -337,  -337,   206,   419,   224,   141,  -337,   209,  -337,
     221,   465,   465,   218,   250,  -337,   176,   252,   553,   465,
     465,  -337,   465,    89,  -337,   519,  -337,     1,   107,    57,
    -337,   245,    89,  -337,  -337,  -337,  -337,  -337,  -337,    89,
     226,   120,  -337,   225,     1,   228,  -337,   230,   227,  -337,
     394,  -337,  -337,  -337,  -337,  -337,   141,  -337,  -337,  -337,
    -337,  -337,  -337,  -337,   141,   465,   129,   206,   465,   536,
    -337,   231,   243,   239,   465,   519,    89,  -337,   519,  -337,
    -337,   240,  -337,   241,  -337,   186,  -337,     1,   191,   255,
     267,  -337,    89,   198,  -337,   159,  -337,   465,  -337,  -337,
    -337,   242,  -337,   206,   145,   249,   254,   248,   286,    -1,
      89,   465,   465,    89,   257,   291,  -337,   465,  -337,  -337,
    -337,  -337,  -337,   176,  -337,  -337,  -337,  -337,  -337,   159,
    -337,  -337,   129,   258,  -337,  -337,   230,  -337,   129,  -337,
     519,   129,  -337,  -337,  -337,  -337,   465,   266,   277,   304,
     309,   570,   519,   243,  -337,  -337,   280,   279,  -337,  -337,
    -337,   249,  -337,   248,   318,   465,   536,   536,   536,  -337,
    -337,  -337,   281,   129,  -337,  -337,  -337,   519,    -1,   317,
      34,   279,  -337,  -337,   465,   536,   282,  -337,   323,  -337,
     536
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -337,  -337,  -337,    26,  -337,   312,  -337,   -19,  -337,    -6,
      -4,  -337,  -337,  -337,   -27,  -336,   -37,   329,   -94,   321,
    -101,  -337,     7,   310,  -337,  -337,  -337,   274,  -337,  -337,
      43,  -337,   -47,   173,  -337,  -337,  -337,   -35,  -337,   -41,
    -337,  -337,  -337,   324,    75,    30,    19,  -337,  -337,  -337,
    -337,  -337,  -202,  -337,  -205,    80,   189,  -200,    13,  -337,
    -215,  -337,  -337,   244,    70,  -238,  -218,  -264,  -337,  -337,
    -234,   134,  -337,  -337,  -337,   146,     3,    42,  -337,  -337,
    -337,  -337,  -337,   -65,  -337,  -337,  -337,  -337,  -337,  -337,
      20,  -337,    -3,  -337,  -337,  -337,  -337,  -337,  -337,  -337,
     -13,  -337,  -337,  -337,  -337,   263,   130,  -337,    77,  -337,
    -337,  -337,  -337,  -170,   -33,   -40,  -337,   319,  -115,  -337,
     275,  -186,  -337,   202,  -247,   155,  -102,  -337,  -337,  -337,
    -337
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -145
static const yytype_int16 yytable[] =
{
       8,   103,   178,   121,   299,    54,   180,   305,    18,   284,
     269,   206,    23,    48,   182,   188,   376,   315,   364,    19,
     264,   287,    34,   256,    39,   258,    50,    62,    51,    23,
      71,   342,   101,    18,   345,    14,   105,    63,    48,   301,
       7,   365,   303,   241,    59,    72,    31,   242,     3,   243,
     110,    50,     7,    81,    34,    13,    42,   376,    99,   100,
     349,   126,   307,     7,   248,   179,    13,    42,    43,    23,
     321,   335,     4,    18,   322,   117,    18,   341,    48,   323,
     344,     9,  -144,  -144,   111,    11,    12,   118,    48,    48,
     253,    50,   190,    81,    18,     7,    13,    48,   137,    16,
      48,    50,    50,    81,    81,    59,   183,  -144,   390,   284,
      50,    20,    81,    50,    80,    51,    34,    74,  -144,   239,
      18,    18,   382,    22,   228,    26,     7,    75,   136,   137,
      44,   235,    45,   403,    30,     7,    28,   299,    13,    42,
      43,    77,    32,    78,    33,   178,   193,     7,   308,    40,
      13,    42,    43,    37,   389,   229,   194,    57,     7,   136,
      58,    13,    42,    43,   207,    48,   195,    73,   399,   341,
     344,    97,   208,    98,   102,   207,   108,   138,    50,   402,
      81,   104,    18,   208,   209,   107,   116,   389,    18,    18,
      18,    18,   402,   111,   -24,   209,   119,   176,   210,    59,
     211,   257,    44,   120,    45,   212,   124,   125,   138,   210,
     236,   211,   266,   240,   185,   351,   212,   241,   213,   214,
     215,   242,   184,   243,   186,   213,   214,   215,   192,   296,
      48,   241,   189,   244,   137,   242,   231,   243,   232,   304,
     238,   137,   245,    50,   249,    81,   247,   251,   310,   252,
     355,   259,   260,   263,   261,   311,   286,    18,   270,   325,
     288,     7,   289,   294,    13,    42,    43,    48,   295,    74,
     298,   136,   271,   272,   273,   274,   275,   309,   312,    75,
      50,   314,    51,   317,   375,   137,   316,   319,    76,   336,
      48,   137,   235,    77,   137,    78,   337,   339,   346,   352,
     357,   347,   358,    50,   361,    81,   360,   363,   353,   371,
      79,   372,    80,   138,   379,   334,   225,   385,   386,   230,
     138,   136,   387,   234,   136,   325,   367,   388,    44,   235,
      45,   381,   392,    48,   325,   393,   397,   405,   400,    48,
     409,   410,    48,    41,   395,    24,    50,    35,    51,    70,
     109,   350,    50,   406,    51,    50,   137,    51,   401,   254,
     407,    60,   374,   370,   138,   324,   325,   137,   137,   380,
     138,   343,   300,   138,    48,   348,   391,   233,   293,   373,
     396,   383,   137,   137,   137,   404,   136,    50,   205,    51,
     354,   313,   106,   137,   255,   191,    18,     0,   136,   265,
       7,   137,   306,    13,    42,    43,   137,   111,   236,     0,
       0,     0,   334,   334,   334,     0,     0,   207,     0,     0,
     290,   290,     0,   136,     0,   208,     0,     0,     0,   302,
       0,   334,     0,     0,     0,   138,   334,   209,     0,     0,
       0,     0,     0,     0,     0,     0,   138,   138,   277,     0,
       0,   210,   320,   211,     0,     0,     0,     0,   212,     0,
       0,   138,   138,   138,     0,     0,     0,   213,   214,   215,
       0,     7,   138,     0,    13,    42,    43,   329,     0,     0,
     138,     0,     0,   340,     0,   138,     0,     0,   207,     0,
       0,     0,   213,   214,   215,     0,   208,   278,   279,   280,
     281,   282,   283,     0,     0,     0,   356,     0,   209,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     368,   369,   210,     0,   211,     7,   290,     0,    13,   212,
       0,     0,     0,     0,   102,   127,     0,     0,   213,   214,
     215,     0,     7,     0,   128,    13,   129,   130,     0,     0,
       0,   102,   127,   131,   132,   384,     0,     0,   133,     7,
       0,   330,     0,   129,   331,     0,   134,   135,   102,   127,
     131,   132,     0,     0,   398,   133,     7,     0,   128,     0,
     129,   130,     0,   332,   333,   102,   127,   131,   132,     0,
       0,     0,   133,   408,     0,   330,     0,   129,   331,     0,
     134,   135,     0,     0,   131,   132,     0,     0,     0,   133,
       0,     0,     0,     0,     0,     0,     0,   332,   333
};

static const yytype_int16 yycheck[] =
{
       4,    66,   104,    97,   238,    32,   107,   245,    12,   224,
     212,   126,    16,    32,   108,   116,   352,   264,    19,    12,
     206,   226,    26,   193,    28,   195,    32,    26,    32,    33,
      49,   295,    65,    37,   298,     9,    69,    36,    57,   239,
       6,    42,   242,    55,    37,    49,    20,    59,     0,    61,
      16,    57,     6,    57,    58,     9,    10,   393,    62,    63,
     307,   101,   248,     6,   179,   105,     9,    10,    11,    73,
     270,   289,    37,    77,   276,    79,    80,   295,    97,   284,
     298,    30,    20,    21,    77,    56,    57,    80,   107,   108,
     184,    97,   119,    97,    98,     6,     9,   116,   102,    17,
     119,   107,   108,   107,   108,    98,   110,    45,   372,   324,
     116,    53,   116,   119,    57,   119,   120,    14,    56,    57,
     124,   125,   360,    56,   128,    44,     6,    24,   102,   133,
      73,   135,    75,   397,    58,     6,    53,   371,     9,    10,
      11,    38,    78,    40,    56,   247,    26,     6,   249,    56,
       9,    10,    11,    46,   372,   129,    36,    78,     6,   133,
      56,     9,    10,    11,    23,   184,    46,    56,   386,   387,
     388,    52,    31,    56,    15,    23,    32,   102,   184,   397,
     184,    56,   186,    31,    43,    59,    32,   405,   192,   193,
     194,   195,   410,   186,    54,    43,    54,    61,    57,   192,
      59,   194,    73,    56,    75,    64,    57,    57,   133,    57,
     135,    59,    60,    51,    21,   309,    64,    55,    73,    74,
      75,    59,    52,    61,    56,    73,    74,    75,    56,    53,
     249,    55,    58,    21,   238,    59,    57,    61,    57,   243,
      52,   245,    56,   249,    53,   249,    56,    60,   252,    52,
     315,    52,    58,    58,    56,   259,    32,   261,    57,   286,
      51,     6,    41,    45,     9,    10,    11,   286,    18,    14,
      18,   245,    66,    67,    68,    69,    70,    32,    52,    24,
     286,    56,   286,    53,   349,   289,    58,    60,    33,    58,
     309,   295,   296,    38,   298,    40,    53,    58,    58,    32,
      58,    60,    53,   309,    56,   309,    52,    21,   312,    52,
      55,    20,    57,   238,    56,   289,   127,    51,    41,   130,
     245,   295,    18,   134,   298,   352,   330,    18,    73,   333,
      75,   358,    52,   352,   361,    56,    18,    20,    57,   358,
      58,    18,   361,    31,   381,    16,   352,    26,   352,    39,
      76,   308,   358,   400,   358,   361,   360,   361,   393,   186,
     401,    37,   343,   333,   289,   285,   393,   371,   372,   356,
     295,   296,   238,   298,   393,   305,   373,   133,   232,   337,
     383,   361,   386,   387,   388,   398,   360,   393,   125,   393,
     313,   261,    73,   397,   192,   120,   400,    -1,   372,   210,
       6,   405,   247,     9,    10,    11,   410,   400,   333,    -1,
      -1,    -1,   386,   387,   388,    -1,    -1,    23,    -1,    -1,
     231,   232,    -1,   397,    -1,    31,    -1,    -1,    -1,   240,
      -1,   405,    -1,    -1,    -1,   360,   410,    43,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   371,   372,    29,    -1,
      -1,    57,    58,    59,    -1,    -1,    -1,    -1,    64,    -1,
      -1,   386,   387,   388,    -1,    -1,    -1,    73,    74,    75,
      -1,     6,   397,    -1,     9,    10,    11,   288,    -1,    -1,
     405,    -1,    -1,   294,    -1,   410,    -1,    -1,    23,    -1,
      -1,    -1,    73,    74,    75,    -1,    31,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,   317,    -1,    43,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     331,   332,    57,    -1,    59,     6,   337,    -1,     9,    64,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    73,    74,
      75,    -1,     6,    -1,    25,     9,    27,    28,    -1,    -1,
      -1,    15,    16,    34,    35,   366,    -1,    -1,    39,     6,
      -1,    25,    -1,    27,    28,    -1,    47,    48,    15,    16,
      34,    35,    -1,    -1,   385,    39,     6,    -1,    25,    -1,
      27,    28,    -1,    47,    48,    15,    16,    34,    35,    -1,
      -1,    -1,    39,   404,    -1,    25,    -1,    27,    28,    -1,
      47,    48,    -1,    -1,    34,    35,    -1,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,    48
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    88,    89,     0,    37,   216,   217,     6,    97,    30,
     201,    56,    57,     9,    90,    91,    17,   202,    97,   109,
      53,    92,    56,    97,   104,   203,    44,   205,    53,   110,
      58,    90,    78,    56,    97,   106,   206,    46,   208,    97,
      56,    92,    10,    11,    73,    75,    90,    93,    94,    95,
      96,    97,    99,   100,   101,   104,   204,    78,    56,   109,
     130,   209,    26,    36,   190,   191,   211,   213,   214,   215,
     110,    94,    97,    56,    14,    24,    33,    38,    40,    55,
      57,    97,   101,   105,   107,   108,   111,   112,   113,   114,
     115,   118,   127,   128,   129,   106,   207,    52,    56,    97,
      97,   201,    15,   170,    56,   201,   204,    59,    32,   114,
      16,   109,   119,   120,   122,   123,    32,    97,   109,    54,
      56,   105,   130,   210,    57,    57,   202,    16,    25,    27,
      28,    34,    35,    39,    47,    48,    90,    97,   131,   134,
     135,   136,   137,   138,   150,   152,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   174,   175,   176,   180,   181,   182,
     183,   184,   185,   186,   188,   189,    61,   212,   213,   202,
     107,   116,   105,    97,    52,    21,    56,   121,   107,    58,
     101,   207,    56,    26,    36,    46,   109,   192,   193,   194,
     196,   197,   198,   199,   200,   192,   205,    23,    31,    43,
      57,    59,    64,    73,    74,    75,    94,    96,    97,    98,
     131,   139,   140,   141,   142,   143,   147,   149,    97,    90,
     143,    57,    57,   150,   143,    97,   131,   132,    52,    57,
      51,    55,    59,    61,    21,    56,   151,    56,   205,    53,
     117,    60,    52,   105,   120,   210,   200,   109,   200,    52,
      58,    56,   195,    58,   208,   143,    60,   143,   144,   139,
      57,    66,    67,    68,    69,    70,   146,    29,    78,    79,
      80,    81,    82,    83,   147,   148,    32,   141,    51,    41,
     143,   162,   164,   162,    45,    18,    53,   133,    18,   157,
     158,   144,   143,   144,    97,   152,   212,   208,   107,    32,
      97,    97,    52,   193,    56,   211,    58,    53,   145,    60,
      58,   144,   139,   141,   142,   101,   102,   177,   178,   143,
      25,    28,    47,    48,    90,   153,    58,    53,   163,    58,
     143,   153,   154,   131,   153,   154,    58,    60,   151,   211,
     117,   105,    32,    97,   195,   170,   143,    58,    53,   103,
      52,    56,   179,    21,    19,    42,   187,    97,   143,   143,
     132,    52,    20,   164,   133,   170,   102,   124,   125,    56,
     145,   101,   152,   177,   143,    51,    41,    18,    18,   153,
     154,   163,    52,    56,   126,   103,   179,    18,   143,   153,
      57,   124,   153,   154,   187,    20,   119,   126,   143,    58,
      18
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
#line 1091 "pascal.y"
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
#line 1121 "pascal.y"
    { 
      tree = (yyvsp[(2) - (2)]); 
      generate_instruction (EXIT, new_iconstant (0), NULL, NULL);
    ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 1135 "pascal.y"
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
#line 1153 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_unsigned_integer_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 1161 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 1163 "pascal.y"
    {
      (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 1170 "pascal.y"
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
#line 1193 "pascal.y"
    { (yyval) = new_tnode (_PLUS, new_attribute_set (0)); ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 1195 "pascal.y"
    { (yyval) = new_tnode (_DASH, new_attribute_set (0)); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 1200 "pascal.y"
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
#line 1220 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_s_attribute (attributes, "name", strdup (yytext));
      Node *node = new_tnode (_IDENTIFIER, attributes);
      (yyval) = node;
    ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 1237 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 1239 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 1241 "pascal.y"
    { (yyval) = new_tnode (_NIL, new_attribute_set (0)); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 1243 "pascal.y"
    { (yyval) = new_tnode (_TRUE, new_attribute_set (0)); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 1245 "pascal.y"
    { (yyval) = new_tnode (_FALSE, new_attribute_set (0)); ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 1250 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_signed_number, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 1258 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_signed_identifier, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 1266 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 1268 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 1270 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 1272 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 1274 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 1279 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_constant_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 1287 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 1289 "pascal.y"
    {
      (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 1296 "pascal.y"
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
#line 1312 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 1314 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 1316 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 1321 "pascal.y"
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
#line 1341 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 1343 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 1345 "pascal.y"
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
#line 1365 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_scalar_type, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 1373 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_idlist, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 1381 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 1383 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 1392 "pascal.y"
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
#line 1445 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 1447 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 1452 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_packed_structured_type, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 1460 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1462 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1464 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 1466 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 1473 "pascal.y"
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
#line 1511 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_simple_type_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 1519 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 1521 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 1528 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_record_type, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 1549 "pascal.y"
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
#line 1560 "pascal.y"
    {
      (yyval) = (yyvsp[(2) - (2)]);
      set_child ((yyval), 0, cons ((yyvsp[(1) - (2)]), get_child ((yyval), 0)));
    ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 1565 "pascal.y"
    { 
      (yyval) = new_interior_node (_field_list, NULL, 2, new_epsilon (), (yyvsp[(1) - (1)]));
    ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 1572 "pascal.y"
    { 
      Node *left = new_epsilon ();
      Node *right = new_epsilon ();
      (yyval) = new_interior_node (_field_list, NULL, 2, left, right);
    ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 1578 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 1583 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_record_section, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1591 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant_part, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 1599 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant, attributes, 2, (yyvsp[(1) - (5)]), (yyvsp[(4) - (5)]));
    ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 1604 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1612 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variant_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 1620 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 1622 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 1629 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_set_type, attributes, 1, (yyvsp[(3) - (3)]));
    ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 1639 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_file_type, attributes, 1, (yyvsp[(3) - (3)]));
    ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 1649 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_pointer_type, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 1659 "pascal.y"
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
#line 1701 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 1703 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 1705 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 1710 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variable_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1718 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 1720 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 1727 "pascal.y"
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
#line 1754 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1756 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 1763 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_indexed_variable, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1773 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_field_designator, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 1788 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_referenced_variable, attributes, 1, (yyvsp[(1) - (2)]));
    ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1798 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1800 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1802 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1804 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1806 "pascal.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1808 "pascal.y"
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
#line 1821 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_set, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1826 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_empty_set, attributes, 0);
    ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1834 "pascal.y"
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
#line 1880 "pascal.y"
    {
      (yyval) = (yyvsp[(1) - (1)]);
    ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1887 "pascal.y"
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
#line 1926 "pascal.y"
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
#line 1938 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1943 "pascal.y"
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
#line 1972 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 2028 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_expr_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 2036 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 2038 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 2049 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _STAR, attributes);
    ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 2054 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _SLASH, attributes);
    ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 2059 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _DIV, attributes);
    ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 2064 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _MOD, attributes);
    ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 2069 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _AND, attributes);
    ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 2079 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _PLUS, attributes);
    ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 2084 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _DASH, attributes);
    ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 2089 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _OR, attributes);
    ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 2099 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _LT, attributes);
    ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 2104 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _LE, attributes);
    ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 2109 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _EQ, attributes);
    ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 2114 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _NE, attributes);
    ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 2119 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _GE, attributes);
    ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 2124 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _GT, attributes);
    ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 2129 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _IN, attributes);
    ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 2143 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_call, attributes, 1, (yyvsp[(1) - (3)]));
    ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 2148 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_call, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 2166 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_statement_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 2174 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 2176 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 2181 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 2183 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 2188 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 2190 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 2195 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 2197 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 2202 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (1);
      Instruction *instruction = get_p_attribute((yyvsp[(3) - (3)])->attributes, "instruction");
      set_p_attribute (attributes, "instruction", instruction);
      (yyval) = new_interior_node (_labeled_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 2212 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_labeled_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 2220 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 2222 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 2227 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 2234 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 2236 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 2238 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 2240 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 2242 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 2254 "pascal.y"
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
#line 2299 "pascal.y"
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
#line 2339 "pascal.y"
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
#line 2361 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_write_parameter_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 2369 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 2371 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));  ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 2376 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 2383 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_call, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 2388 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_call, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 2398 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_goto_statement, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 2408 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_empty_statement, attributes, 0);
    ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 2418 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 2420 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 2422 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 2424 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 2429 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 2431 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 2433 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 2440 "pascal.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 2447 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 2449 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 2454 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 2456 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 2463 "pascal.y"
    {
      /* Generate labels for consequent and end. */
      generate_label (CONSEQUENT_LABEL);
      generate_label (END_LABEL);
      
      /* Generate control flow instructions. */
      translate_bexpr ((yyvsp[(2) - (4)]), consequent_label, end_label);
      
      generate_instruction (_LABL, consequent_label, NULL, NULL);
      
      /* Code for the consequent */
      Instruction *instruction = get_p_attribute ((yyvsp[(4) - (4)])->attributes, "instruction");
      generate_statement (instruction);
      
      generate_instruction (_LABL, end_label, NULL, NULL);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 2486 "pascal.y"
    { 
      /* Generate labels for consequent, alternate, and end. */
      generate_label (CONSEQUENT_LABEL);
      generate_label (ALTERNATE_LABEL);
      generate_label (END_LABEL);

      /* Generate control flow instructions. */
      translate_bexpr ((yyvsp[(2) - (6)]), consequent_label, alternate_label);

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

  case 162:

/* Line 1455 of yacc.c  */
#line 2518 "pascal.y"
    {
      /* Generate labels for consequent, alternate, and end. */
      generate_label (CONSEQUENT_LABEL);
      generate_label (ALTERNATE_LABEL);
      generate_label (END_LABEL);

      /* Generate control flow instructions. */
      translate_bexpr ((yyvsp[(2) - (6)]), consequent_label, alternate_label);

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

  case 163:

/* Line 1455 of yacc.c  */
#line 2552 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_case_statement, attributes, 2, (yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]));
    ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 2560 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_case_element, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 2568 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_case_element_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 2576 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 2578 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 2585 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 2587 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 2589 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 2594 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 172:

/* Line 1455 of yacc.c  */
#line 2596 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 2603 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_while_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 174:

/* Line 1455 of yacc.c  */
#line 2611 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_while_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 2621 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 2631 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_for_statement, attributes, 5, 
                              (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(8) - (8)]));
    ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 2640 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_for_statement, attributes, 5, 
                              (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(8) - (8)]));
    ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 2649 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_direction, _TO, attributes);
    ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 2654 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_direction, _DOWNTO, attributes);
    ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 2664 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 2672 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 2688 "pascal.y"
    {
      /* Build an array of procedure children nodes. */
      Node *procedure_children[] = {(yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)])};
      
      /* Generate instructions for all parts of procedure. */
      generate_procedure_function (procedure_children);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_declaration, attributes, 7,
                              (yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
    ;}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 2703 "pascal.y"
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

  case 184:

/* Line 1455 of yacc.c  */
#line 2757 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 2759 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 2764 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 2766 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 2768 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 2770 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 2775 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_formals_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 2783 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 2785 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 2790 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_value_parameter, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 2798 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variable_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 2806 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 2814 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 2822 "pascal.y"
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

  case 198:

/* Line 1455 of yacc.c  */
#line 2854 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 2857 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_label_declaration_part, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 2865 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 2868 "pascal.y"
    {  
     (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 2874 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_constant_definition_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 2882 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 2884 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 2888 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 2890 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 2895 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_type_definition_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 2903 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 2905 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 2910 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 2912 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 2917 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variable_declaration_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 2925 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 2927 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 2932 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 2934 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_procedure_declaration_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 2942 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 2944 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 2949 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 2951 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
#line 2964 "pascal.y"
    {
      /* Build an array of procedure children nodes. */
      Node *function_children[] = {(yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)])};
      
      /* Generate instructions for all parts of function. */
      generate_procedure_function (function_children);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_declaration, attributes, 7,
                              (yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
    ;}
    break;

  case 222:

/* Line 1455 of yacc.c  */
#line 2979 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_heading, attributes, 3, (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(7) - (8)]));
    ;}
    break;

  case 223:

/* Line 1455 of yacc.c  */
#line 2996 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program, attributes, 7,
                              (yyvsp[(1) - (8)]), (yyvsp[(2) - (8)]), (yyvsp[(3) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(7) - (8)]));
    ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 3005 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 0);
    ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 3010 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 3015 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 2, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]));
    ;}
    break;



/* Line 1455 of yacc.c  */
#line 5292 "pascal.tab.c"
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
#line 3021 "pascal.y"


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
translate_bexpr (Node *node, StacParameter *true_label, StacParameter *false_label)
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
} // translate_bexpr


/* Our beautiful lexer. */
#include "lex.yy.c"

/* Code for handling attributes. */
#include "attribute.c"

/* Code for building parse trees. */
#include "parse-tree.c"

