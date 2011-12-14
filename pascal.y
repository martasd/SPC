/**
 * file:        pascal.y
 * description: A simple Pascal parser
 * author:      Sam Rebelsky
 * modified by: Martin Dluhos
 * revised:     December 11, 2011
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
 
/* Headers that we rely upon. */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "attribute.h"
#include "parse-tree.h"
#include "symtab.h"
#include "stac.h"
%}

/* Since we are building a parse tree, we want each element to have
 * an associated parse tree.
 */
%{
#define YYSTYPE Node *
%}

/* Look!  Tokens. */
%token TOKENS_START
%token _ERROR
%token GENERAL_START
%token _IDENTIFIER
%token GENERAL_END
%token CONSTANTS_START
%token _INTEGER
%token _REAL
%token _STRING
%token CONSTANTS_END
%token KEYWORDS_START
%token _ARRAY
%token _BEGIN
%token _CASE
%token _CONST
%token _DO
%token _DOWNTO
%token _ELSE
%token _END
%token _EOL
%token _FALSE
%token _FILE
%token _FOR
%token _FUNCTION
%token _GOTO
%token _IF
%token _IN
%token _LABEL
%token _NIL
%token _OF
%token _PACKED
%token _WRITE
%token _WRITELN
%token _PROCEDURE
%token _PROGRAM
%token _RECORD
%token _REPEAT
%token _SET
%token _THEN
%token _TO
%token _TRUE
%token _TYPE
%token _UNTIL
%token _VAR
%token _WHILE
%token _WITH
%token KEYWORDS_END
%token PUNCTUATION_START
%token _ASSIGN
%token _COLON
%token _COMMA
%token _ELLIPSES
%token _POINTER
%token _SEMICOLON
%token _LPAREN
%token _RPAREN
%token _LBRACKET
%token _RBRACKET
%token _DOT
%token PUNCTUATION_END
%token OPERATORS_START
%token _NOT
%token MULOPS_START
%token _AND
%token _DIV
%token _MOD
%token _SLASH
%token _STAR
%token MULOPS_END
%token ADDOPS_START
%token _DASH
%token _OR
%token _PLUS
%token ADDOPS_END
%token RELOPS_START
%token _EQ
%token _GE
%token _GT
%token _LE
%token _LT
%token _NE
%token RELOPS_END
%token OPERATORS_END
%token TOKENS_END

%{
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



/* Copied from pascal.y by Sam Rebelsky. */
OpCode get_arithmetic_opcode (int operator, TypeID type_id);
OpCode get_boolean_opcode (int operator, TypeID type_id);
OpCode get_assignment_opcode (TypeID type_id);
int is_arithmetic_operator (int operator);
int is_boolean_operator (int operator);
int is_assignment_operator (int operator);
OpCode get_opcode (int operator, TypeID operand);
StacParameter *translate_expr (int operator, Node *left, Node *right);

void
init_symtab ()
{
}
/* Declare the symbol table for the program. */
SymTab *stab;
%}

%%

	/* The start of our grammar.  This is normally a program.
	   However, we may set it to other things during testing. */

start
  : 
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
    }
    program
    { 
      tree = $2; 
      generate_instruction (EXIT, new_iconstant (0), NULL, NULL);
    }
  ;

	/* The remaining parts of our grammar are in the same order
	   as in the 1973 Pascal report.  Some of those nonterminals
	   are done as lexical rules. */

	/* Section 4. */

unsigned_integer 
  : _INTEGER
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
      $$ = node;
    }
  ;

unsigned_integer_list
  : unsigned_integer unsigned_integer_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_unsigned_integer_list, attributes, cons ($1, $2));
    }
  ;

unsigned_integer_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA unsigned_integer unsigned_integer_list_tail
    {
      $$ = cons ($2, $3);
    }
  ;

unsigned_real
  : _REAL
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
      $$ = node;
    }
  ;

unsigned_number
  : unsigned_integer
  | unsigned_real
  ;

sign 
  : _PLUS
    { $$ = new_tnode (_PLUS, new_attribute_set (0)); }
  | _DASH
    { $$ = new_tnode (_DASH, new_attribute_set (0)); }
  ;

string
  : _STRING
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
      $$ = node;
    }
  ;

        /* Section 5. */

id
  : _IDENTIFIER
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_s_attribute (attributes, "name", strdup (yytext));
      Node *node = new_tnode (_IDENTIFIER, attributes);
      $$ = node;
    }
  ;

        /* Unsigned constants are supposed to include identifiers.
         * However, we have two rules for factor, both of which
         * lead to an identifier.  
         *   factor: unsigned_constant
         *   factor: variable
         * This seems to be the safest place to cut.
         */
unsigned_constant
  : unsigned_number
    { $$ = $1; }
  | string
    { $$ = $1; }
  | _NIL
    { $$ = new_tnode (_NIL, new_attribute_set (0)); }
  | _TRUE
    { $$ = new_tnode (_TRUE, new_attribute_set (0)); }
  | _FALSE
    { $$ = new_tnode (_FALSE, new_attribute_set (0)); }
  ;

signed_number
  : sign unsigned_number
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_signed_number, attributes, 2, $1, $2);
    }
  ;

signed_identifier
  : sign id
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_signed_identifier, attributes, 2, $1, $2);
    }
  ;

constant
  : unsigned_number
    { $$ = $1; }
  | signed_number
    { $$ = $1; }
  | id
    { $$ = $1; }
  | signed_identifier
    { $$ = $1; }
  | string
    { $$ = $1; }
  ;

constant_list
  : constant constant_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_constant_list, attributes, cons ($1, $2));
    }
  ;

constant_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA constant constant_list_tail
    {
      $$ = cons ($2, $3);
    }
  ;

constant_definition
  : id _EQ constant
    {
      /* Put the new constant in the symbol table. */
      AttributeSet *attributes = new_attribute_set (1);
   
      /* Generate code for constant definition. */
      StacParameter *address = translate_expr (_EQ, $1, $3);
      set_p_attribute (attributes, "address", address);
      
      $$ = new_interior_node (_constant_definition, attributes, 2, $1, $3);
    }
  ;

        /* Section 6. Data type definitions */

type
  : simple_type
    { $$ = $1; }
  | structured_type
    { $$ = $1; }
  | pointer_type
    { $$ = $1; }
  ;

type_definition
  : id _EQ type
    {
      /* Get the name of the id. */
      char *id_name = get_s_attribute ($1->attributes, "name");

      /* Put the new type in the symbol table. */
      TypeID alias_type = get_type_id ($3);
      Type *type = new_type (alias_type);
      AttributeSet *type_attributes = new_attribute_set (1);
      set_p_attribute (type_attributes, "type", type);
      symtab_put (stab, id_name, type_attributes);
      
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_type_definition, attributes, 2, $1, $3);
    }
  ;

        /* Section 6.1. Simple types */

simple_type
  : scalar_type
    { $$ = $1; }
  | subrange_type
    { $$ = $1; }
  | id
    { 
      /* Get the name of the id (new type) from its attributes. */
      char *id_name = get_s_attribute ($1->attributes, "name");

      /* Create a new type struct. */
      AttributeSet *id_attributes = symtab_get (stab, id_name);
      Type *type = get_p_attribute (id_attributes, "type");
      if (type == NULL)
        fprintf (stderr, "Not a valid type name.\n");

      AttributeSet *attributes = new_attribute_set (1);
      set_p_attribute (attributes, "type", type);
      $$ = new_interior_node (_simple_type, attributes, 1, $1);
    }
  ;

        /* Section 6.1.1. Scalar types */

scalar_type
  : _LPAREN idlist _RPAREN
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_scalar_type, attributes, 1, $2);
    }
  ;

idlist
  : id idtail
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_idlist, attributes, cons ($1, $2));
    }
  ;   

idtail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA id idtail
    { $$ = cons ($2, $3); }
  ;

        /* Section 6.1.2. Standard scalar types */

        /* Section 6.1.3. Subrange types */

subrange_type
  : constant _ELLIPSES constant
    {
      /* Create a type for the subrange type. */
      struct SubrangeType *subrange_struct = 
                                   malloc (sizeof (struct SubrangeType));

      
      /* If bounds are incompatible, then throw an appropriate error. */
      if (types_compatible ($1, $3) == 0)
        fprintf (stderr, "Incompatible types in subrange!\n");

      printf ("in subrange\n");
      /* Types are compatible, so assign the type to subrange. */
      Type *const_type = get_p_attribute ($1->attributes, "type");
      subrange_struct->type = const_type;

      /* Retrieve the constant values based on the type. */
      Attribute lower;
      Attribute upper;

      switch (const_type->type_id)
	{
	case (TYPE_INTEGER):
	  lower.ival = get_i_attribute ($1->attributes, "ivalue");
	  upper.ival = get_i_attribute ($3->attributes, "ivalue");
	  break;
	case (TYPE_REAL):
	  lower.dval = get_r_attribute ($1->attributes, "rvalue");
	  upper.dval = get_r_attribute ($3->attributes, "rvalue");
	  break;
	case (TYPE_STRING):
	  lower.sval = get_s_attribute ($1->attributes, "svalue");
	  upper.sval = get_s_attribute ($3->attributes, "svalue");
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
      $$ = new_interior_node (_subrange_type, subrange_attributes, 2, $1, $3);
    }
  ;

        /* Section 6.2. Structured types */

structured_type
  : unpacked_structured_type
    { $$ = $1; }
  | packed_structured_type
    { $$ = $1; }
  ;

packed_structured_type
  : _PACKED unpacked_structured_type
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_packed_structured_type, attributes, 1, $2);
    }
  ;

unpacked_structured_type
  : array_type
    { $$ = $1; }
  | record_type
    { $$ = $1; }
  | set_type
    { $$ = $1; }
  | file_type
    { $$ = $1; }
  ;

        /* Section 6.2.1. Array Types */

array_type
  : _ARRAY _LBRACKET simple_type_list _RBRACKET _OF type
    {
      int i;

      /* Create a type for the array type. */
      struct ArrayType *array_struct = malloc (sizeof (struct ArrayType));

      /* Get the component type. */
      array_struct->component_type = get_p_attribute ($6->attributes, "type");

      /* In this implementation, all arrays are one-dimensional. */
      array_struct->dimensions = 1;

      /* Determine how many types to process. */
      int num_types = get_arity ($3);

      /* Allocate space for an array of index types. */
      array_struct->index_types = malloc (sizeof (struct Type) * num_types);

      /* Process simple_type_list. */
      for (i = 0; i < num_types; i++)
      {
        /* Get the type from type_list and store it in the array. */
        Node *type_node = get_child ($3, i);
        Type *type = get_p_attribute (type_node->attributes,"type");
        array_struct->index_types[i] = type;
      }

      /* Store the array attributes with the node. */
      AttributeSet *array_attributes = new_attribute_set (1);
      Type *type = new_type (TYPE_ARRAY);
      type->info.array = array_struct;
      set_p_attribute (array_attributes, "type", type);
      $$ = new_interior_node (_array_type, array_attributes, 2, $3, $6);
    }
  ;

simple_type_list
  : simple_type simple_type_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_simple_type_list, attributes, cons ($1, $2));
    }
  ;

simple_type_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA simple_type simple_type_list_tail
    { $$ = cons ($2, $3); }
  ;

        /* Section 6.2.2. Record types */

record_type
  : _RECORD field_list _END
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_record_type, attributes, 1, $2);
    }
  ;

        /* When the Wirth grammar is turned into standard grammar form
         * that is, no { SEMICOLON record_section } for Kleene star, we 
         * have a shift-reduce conflict in the fixed part.  In particular,
         * xince fixed parts can be followed by a semicolon, if we see 
         * a semicolon after a record section, it's not clear whether 
         * we've finished the fixed part or are ready to read another 
         * record section.  This is my less-than elegant solution.
         *
         * The kernel builds a pair (with symbol field list), which child
         * 0 containing the fixed part (a list of record sections)  and
         * child 1 containing the variant section.  If either part is 
         * missing in the source text, it is represented as epsilon.
         */
field_list
  : field_list_kernel
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = $1;
      $$->attributes = attributes;
      Node *fields = get_child ($$, 0);
      set_child ($$, 0, list2node (_fixed_part, NULL, fields));
    }
  ;

field_list_kernel
  : record_section field_list_tail
    {
      $$ = $2;
      set_child ($$, 0, cons ($1, get_child ($$, 0)));
    }
  | variant_part
    { 
      $$ = new_interior_node (_field_list, NULL, 2, new_epsilon (), $1);
    }
  ; 

field_list_tail
  : /* epsilon */
    { 
      Node *left = new_epsilon ();
      Node *right = new_epsilon ();
      $$ = new_interior_node (_field_list, NULL, 2, left, right);
    }
  | _SEMICOLON field_list_kernel
    { $$ = $2; }
  ;

record_section
  : idlist _COLON type
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_record_section, attributes, 2, $1, $3);
    }
  ;

variant_part
  : _CASE id _COLON id _OF variant_list
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_variant_part, attributes, 3, $2, $4, $6);
    }
  ;

variant
  : constant_list _COLON _LPAREN field_list _RPAREN
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_variant, attributes, 2, $1, $4);
    }
  | constant_list
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_variant, attributes, 1, $1);
    }
  ;

variant_list
  : variant variant_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_variant_list, attributes, cons ($1, $2));
    }
  ;

variant_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _SEMICOLON variant variant_list_tail
    { $$ = cons ($2, $3); }
  ;

        /* Section 6.2.3. Set types */

set_type
  : _SET _OF simple_type
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_set_type, attributes, 1, $3);
    }
  ;

        /* Section 6.2.4. File types */

file_type
  : _FILE _OF type
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_file_type, attributes, 1, $3);
    } 
  ;

        /* Section 6.3. Pointer types */

pointer_type
  : _POINTER id
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_pointer_type, attributes, 1, $2);
    }
  ;

        /* Section 7. Declarations and denotations of variables */

variable_declaration
  : idlist _COLON type
    {
      int i;
   
      /* Determine how many ids to process. */
      int num_ids = get_arity ($1);
      
      /* Put each id with its corresponding type in the symbol table. */
      for (i = 0; i < num_ids; i++)
      {                                                                              
        /* Get the name of the id from its attributes. */
        Node *id = get_child ($1, i);
        char *id_name = get_s_attribute (id->attributes, "name");
        
        /* Set the type. */
        AttributeSet *sym_attributes = new_attribute_set (2);
        TypeID type_id = get_type_id ($3);
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
      $$ = new_interior_node (_variable_declaration, node_attributes, 2, $1, $3);
    }
  ;

variable
  : entire_variable
    { $$ = $1; }
  | component_variable
    { $$ = $1; }
  | referenced_variable
    { $$ = $1; }
  ;

variable_list
  : variable variable_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_variable_list, attributes, cons ($1, $2));
    }
  ;

variable_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA variable variable_list_tail
    { $$ = cons ($2, $3); }
  ;

        /* Section 7.1. Entire variables */

entire_variable
  : id 
    {
      stop_here ();
      
      /* Retrieve the id attributes from symbol table. */
      char *id_name = get_s_attribute ($1->attributes, "name");
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

      $$ = new_interior_node (_entire_variable, attributes, 1, $1);
    }
  ;

        /* Section 7.2. Component variables */

component_variable
  : indexed_variable
    { $$ = $1; }
  | field_designator
    { $$ = $1; }
  ;

        /* Section 7.2.1. Indexed variables */

indexed_variable
  : variable _LBRACKET expr_list _RBRACKET
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_indexed_variable, attributes, 2, $1, $3);
    }
  ;

        /* Section 7.2.2. Field designators */

field_designator
  : variable _DOT id
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_field_designator, attributes, 2, $1, $3);
    }
  ;

        /* Section 7.2.3. File buffers */
        /* Section 7.3. Referenced variables */

        /* These two types are indistinguishable at the syntactic level.
         * both are variables followed by the pointer symbol.
         */

referenced_variable
  : variable _POINTER
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_referenced_variable, attributes, 1, $1);
    }
  ;

        /* Section 8. Expressions */

factor
  : variable
    { $$ = $1; }
  | unsigned_constant
    { $$ = $1; }
  | function_call
    { $$ = $1; }
  | set
    { $$ = $1; }
  | _LPAREN expr _RPAREN
    { $$ = $2; }
  | _NOT factor
    {
      AttributeSet *attributes = new_attribute_set (1);

      /* A little bit of code. */
      StacParameter *address = translate_expr (_NOT, NULL, $2);
      set_p_attribute (attributes, "address", address);

      $$ = new_interior_node (_negate, attributes, 1, $2);
    }
  ;

set
  : _LBRACKET expr_list _RBRACKET
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_set, attributes, 1, $2);
    }
  | _LBRACKET _RBRACKET
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_empty_set, attributes, 0);
    }
  ;

term
  : term mulop factor
    {
      /* Get the mulop operator. */
      int operator = get_operator ($2);

      /* Determine which operation we are performing and thus determine
       * allowed types. If types are inappropriate, throw an appropriate error.
       */
      switch (operator)
      {
       case (_STAR): 
       case (_SLASH):
       case (_DIV):
         if (types_compatible ($1, $3) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (get_type_id ($1) != TYPE_INTEGER && get_type_id ($1) != TYPE_REAL)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
        break;
       case (_MOD):
         if (types_compatible ($1, $3) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (get_type_id ($1) != TYPE_INTEGER)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
        break;
       case (_AND):
         if (types_compatible ($1, $3) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (get_type_id ($1) != TYPE_BOOLEAN)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
       }

      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (2);
      TypeID type_id = get_type_id ($3);
      Type *type = new_type (type_id);
      set_p_attribute (attributes, "type", type);

      /* Generate some code. */
      StacParameter *address = translate_expr (operator, $1, $3);
      set_p_attribute (attributes, "address", address);

      /* We do not perform operand type conversion in this implementation, 
       * so just pass the children.
       */
      $$ = new_interior_node (_term, attributes, 3, $1, $2, $3);
    }
  | factor
    {
      $$ = $1;
    }
  ;

simple
  : simple addop term
    {
      /* Get the addop operator. */
      int operator = get_operator ($2);

      /* Determine which addop operation we are performing. If addition or 
       * subtraction, then allowed types are integers and reals.
       * If types are inappropriate, throw an appropriate error.
       */
      if ((operator == _PLUS) || (operator == _DASH))
      {
        if (types_compatible ($1, $3) == 0)
          fprintf (stderr, "Incompatible types in expression!\n");
        if (get_type_id ($1) != TYPE_INTEGER && get_type_id ($1) != TYPE_REAL)
          fprintf (stderr, "Operands have incorrect type in expression.\n");
      }
      /* Otherwise, the operator is OR. */
      else 
      {
        if (types_compatible ($1, $3) == 0)
          fprintf (stderr, "Incompatible types in expression!\n");
        if (get_type_id ($1) != TYPE_BOOLEAN)
          fprintf (stderr, "Operands have incorrect type in expression.\n");
      } // if
 
      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (2);
      Type *type = new_type (get_type_id ($3));
      set_p_attribute (attributes, "type", type);
      
      /* Time to generate code. */
      StacParameter *address = translate_expr (operator, $1, $3);
      set_p_attribute (attributes, "address", address);

      /* We do not do operand type conversion in this implementation, 
       * so just pass the children.
       */
      $$ = new_interior_node (_simple, attributes, 3, $1, $2, $3);
    }
  | addop term
  {
    int operator = get_operator ($2);		

    AttributeSet *attributes = new_attribute_set (1);
        
    /* Generate some exquisite code! */
    StacParameter *address = translate_expr (operator, NULL, $2);
    set_p_attribute (attributes, "address", address);

    $$ = new_interior_node (_simple, attributes, 2, $1, $2);
    }
  | term
    { $$ = $1; }
  ;

expr
  : simple relop simple
    {
      int operator = get_operator ($2);
	
      /* If types are incompatible or inappropriate, throw an appropriate 
       * error. 
       */
      if (types_compatible ($1, $3) == 0)
        fprintf (stderr,"Incompatible types in expression!\n"); 
      if (get_type_id ($1) != TYPE_INTEGER && get_type_id ($1) != TYPE_REAL)
        fprintf (stderr, "Operands have incorrect type in expression.\n");

      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (2);
      Type *type = new_type (get_type_id ($3));
      set_p_attribute (attributes, "type", type);

      /* Code generation! */
      StacParameter *address = translate_expr (operator, $1, $3);
      set_p_attribute (attributes, "address", address);

      /* We do not do operand conversion in this implementation, so just pass
       * the children.
       */
      $$ = new_interior_node (_expr, attributes, 3, $1, $2, $3);
    }
  | simple
    { $$ = $1; }
  ;

expr_list
  : expr expr_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_expr_list, attributes, cons ($1, $2));
    }
  ;

expr_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA expr expr_list_tail
    { $$ = cons ($2, $3); }
  ;

        /* Section 8.1. Operators */

        /* Section 8.1.1. The operator not */

        /* Section 8.1.2. Multiplying operators */

mulop
  : _STAR
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_mulop, _STAR, attributes);
    }
  | _SLASH
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_mulop, _SLASH, attributes);
    }
  | _DIV
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_mulop, _DIV, attributes);
    }
  | _MOD
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_mulop, _MOD, attributes);
    }
  | _AND
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_mulop, _AND, attributes);
    }
  ;

        /* Section 8.1.3. Adding operators */

addop
  : _PLUS
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_addop, _PLUS, attributes);
    }
  | _DASH
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_addop, _DASH, attributes);
    }
  | _OR
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_addop, _OR, attributes);
    }
  ;

        /* Section 8.1.4. Relational operators */

relop
  : _LT
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _LT, attributes);
    }
  | _LE
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _LE, attributes);
    }
  | _EQ
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _EQ, attributes);
    }
  | _NE
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _NE, attributes);
    }
  | _GE
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _GE, attributes);
    }
  | _GT
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _GT, attributes);
    }
  | _IN
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_relop, _IN, attributes);
    }
  ;
   
        /* Section 8.2. Function designators */

        /* We're going to change Pascal slightly and require that
         * function calls always have parentheses. 
         */

function_call
  : id  _LPAREN _RPAREN
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_function_call, attributes, 1, $1);
    }
  | id _LPAREN expr_list _RPAREN
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_function_call, attributes, 2, $1, $3);
    }
  ;

        /* Section 9. Statements */

        /* Because if statements are ambiguous, we need to resolve
         * the ambiguity.  We do that by separating statements that
         * are safe to be put as the consequent of an if-then-else 
         * statement (that is, everything but an if-then-no-else statement
         * or a structured statement that has an if-then-no-else statement
         * as its body) and those that are unsafe to use as a consequent.
         */

statement_list
  : statement statement_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_statement_list, attributes, cons ($1, $2));
    }
  ;

statement_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _SEMICOLON statement statement_list_tail
    { $$ = cons ($2, $3); }
  ;

statement
  : safe_statement
    { $$ = $1; }
  | unsafe_statement
    { $$ = $1; }
  ;

safe_statement
  : labeled_safe_statement
    { $$ = $1; }
  | unlabeled_safe_statement
    { $$ = $1; }
  ;

unsafe_statement
  : labeled_unsafe_statement
    { $$ = $1; }
  | unlabeled_unsafe_statement
    { $$ = $1; }
  ;

labeled_safe_statement
  : unsigned_integer _COLON unlabeled_safe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_labeled_statement, attributes, 2, $1, $3);
    }
  ;

labeled_unsafe_statement
  : unsigned_integer _COLON unlabeled_unsafe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_labeled_statement, attributes, 2, $1, $3);
    }
  ;
    
unlabeled_safe_statement
  : simple_statement
    { $$ = $1; }
  | safe_structured_statement
    { $$ = $1; }
  ;

unlabeled_unsafe_statement
  : unsafe_structured_statement
    { $$ = $1; }
  ;

        /* Section 9.1. Simple Statements */

simple_statement
  : assignment_statement
    { $$ = $1; }
  | output_statement
    { $$ = $1; }
  | procedure_call
    { $$ = $1; }
  | goto_statement
    { $$ = $1; }
  | empty_statement
    { $$ = $1; }
  ;

        /* Section 9.1.1. Assignent statements */

        /* We leave out <function_identifier> := <expression>
         * because variables include identifiers.  The run-time
         * code must figure out which rule an identifier serves.
         */

assignment_statement
  : variable _ASSIGN expr
    {
      if (types_compatible ($1, $3) == 0)
        fprintf (stderr, "Expression has incompatible type in assignment!\n");
      
      /* Determine which instruction to use. */
      OpCode opcode;
      switch (get_type_id ($1))
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
      StacParameter *var_address = get_p_attribute ($1->attributes, "address");

      /* Retrieve the address of the expression. */
      StacParameter *expr_address = get_p_attribute ($3->attributes, "address");

      /* Generate code for assignment. */
      generate_instruction (opcode, var_address, expr_address, NULL);

      AttributeSet *attributes = new_attribute_set (0);	  
      $$ = new_interior_node (_assignment_statement, attributes, 2, $1, $3);
    }
  ;

        /* Added a section for Phase 5: The output statement */

output_statement
  : _WRITE _LPAREN write_parameter_list _RPAREN
    {
      int num_write_params = get_arity ($3);

      int i; 
      for (i = 0; i < num_write_params; i++)
      {
        Node *write_param = get_child ($3, i);

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
      $$ = new_interior_node (_output_statement, attributes, 1, $3);
    }
  | _WRITELN _LPAREN write_parameter_list _RPAREN
    {
      int num_write_params = get_arity ($3);

      int i; 
      for (i = 0; i < num_write_params; i++)
      {
        Node *write_param = get_child ($3, i);

        StacParameter *param_address = 
                       get_p_attribute (write_param->attributes, "address");

         /* Generate the code! */
         generate_instruction (WRITELN, param_address, NULL, NULL);
      } // for

      AttributeSet *attributes = new_attribute_set (0);	  
      $$ = new_interior_node (_output_statement, attributes, 1, $3);
    }
  ;

write_parameter_list
  : write_parameter write_parameter_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_write_parameter_list, attributes, cons ($1, $2));
    }
  ;

write_parameter_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _COMMA write_parameter write_parameter_list_tail
    { $$ = cons ($2, $3);  }
  ;
     
write_parameter
  : expr
    { $$ = $1; }
  ;

        /* Section 9.1.2. Procedure calls */

procedure_call
  : id
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_procedure_call, attributes, 1, $1);
    }
  | id _LPAREN expr_list _RPAREN
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_procedure_call, attributes, 2, $1, $3);
    }
  ;

        /* Section 9.1.3. Goto statements */

goto_statement
  : _GOTO unsigned_integer
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_goto_statement, attributes, 1, $2);
    }
  ;

        /* Section 9.1.4. The empty statement */

empty_statement
  :
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_empty_statement, attributes, 0);
    }
  ;

        /* Section 9.2. Structured statements */

safe_structured_statement
  : compound_statement
    { $$ = $1; }
  | safe_conditional_statement
    { $$ = $1; }
  | safe_repetitive_statement
    { $$ = $1; }
  | safe_with_statement
    { $$ = $1; }
  ;

unsafe_structured_statement
  : unsafe_conditional_statement
    { $$ = $1; }
  | unsafe_repetitive_statement
    { $$ = $1; }
  | unsafe_with_statement
    { $$ = $1; }
  ;

	/* Section 9.2.1. Compound statements */

compound_statement
  : _BEGIN statement_list _END
    { $$ = $2; }
  ;

	/* Section 9.2.2. Conditional statements */

safe_conditional_statement
  : safe_if_then_else_statement
    { $$ = $1; }
  | case_statement
    { $$ = $1; }
  ;

unsafe_conditional_statement
  : if_then_statement
    { $$ = $1; }
  | unsafe_if_then_else_statement
    { $$ = $1; }
  ;

        /* Section 9.2.2.1. If statements */

if_then_statement
  : _IF expr _THEN safe_statement
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_if_then_statement, attributes, 2, $2, $4);
    }
  ;

safe_if_then_else_statement
  : _IF expr _THEN safe_statement _ELSE safe_statement
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_if_then_else_statement, attributes, 3, $2, $4, $6);
    }
  ;

unsafe_if_then_else_statement
  : _IF expr _THEN safe_statement _ELSE unsafe_statement
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_if_then_else_statement, attributes, 3, $2, $4, $6);
    }
  ;


        /* 9.2.2.2. Case statements */

case_statement
  : _CASE expr _OF case_element_list _END
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_case_statement, attributes, 2, $2, $4);
    }
  ;

case_element
  : constant_list _COLON statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_case_element, attributes, 2, $1, $3);
    }
  ;

case_element_list
  : case_element case_element_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_case_element_list, attributes, cons ($1, $2));
    }
  ;

case_element_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _SEMICOLON case_element case_element_list_tail
    { $$ = cons ($2, $3); }
  ;

        /* 9.2.3. Repetitive statements */

safe_repetitive_statement
  : safe_while_statement
    { $$ = $1; }
  | repeat_statement
    { $$ = $1; }
  | safe_for_statement
    { $$ = $1; }
  ;

unsafe_repetitive_statement
  : unsafe_while_statement
    { $$ = $1; }
  | unsafe_for_statement
    { $$ = $1; }
  ;

        /* 9.2.3.1. While statements */

safe_while_statement
  : _WHILE expr _DO safe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_while_statement, attributes, 2, $2, $4);
    }
  ;

unsafe_while_statement
  : _WHILE expr _DO unsafe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_while_statement, attributes, 2, $2, $4);
    }
  ;

        /* 9.2.3.2. Repeat statements */

repeat_statement
  : _REPEAT statement_list _UNTIL expr
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_repeat_statement, attributes, 2, $2, $4);
    }
  ;

        /* 9.2.3.3. For statements */

safe_for_statement
  : _FOR id _ASSIGN expr direction expr _DO safe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_for_statement, attributes, 5, 
                              $2, $4, $5, $6, $8);
    }
  ;

unsafe_for_statement
  : _FOR id _ASSIGN expr direction expr _DO unsafe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_for_statement, attributes, 5, 
                              $2, $4, $5, $6, $8);
    }
  ;

direction
  : _TO
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_direction, _TO, attributes);
    }
  | _DOWNTO
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = simple_unary_tree (_direction, _DOWNTO, attributes);
    }
  ;

        /* 9.2.4. With statements */

safe_with_statement
  : _WITH variable_list _DO safe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_repeat_statement, attributes, 2, $2, $4);
    }
  ;

unsafe_with_statement
  : _WITH variable_list _DO unsafe_statement
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_repeat_statement, attributes, 2, $2, $4);
    }
  ;

        /* 10. Procedure declarations */

procedure_declaration
  : procedure_heading 
    label_declaration_part
    constant_definition_part
    type_definition_part
    variable_declaration_part
    procedure_declaration_list
    compound_statement
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_procedure_declaration, attributes, 7,
                              $1, $2, $3, $4, $5, $6, $7);
    }
  ;

procedure_heading
  : _PROCEDURE id _LPAREN formal_parameters _RPAREN _SEMICOLON
    {
      int i;
      Type *param_type;
      char *param_name;
            
      /* Get the name of the id from its attributes. */ 
      char *id_name = get_s_attribute ($2->attributes, "name");

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
      $$ = new_interior_node (_procedure_heading, node_attributes, 2, $2, $4);
    }
  ;

formal_parameters
  : /* epsilon */
    { $$ = new_epsilon (); }
  | formals_list
    { $$ = $1; }
  ;

formals
  : value_parameter
    { $$ = $1; }
  | variable_parameter
    { $$ = $1; }
  | function_parameter
    { $$ = $1; }
  | procedure_parameter
    { $$ = $1; }
  ;

formals_list
  : formals formals_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_formals_list, attributes, cons ($1, $2));
    }
  ;

formals_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | _SEMICOLON formals formals_list_tail
    { $$ = cons ($2, $3); }
  ;

value_parameter
  : parameter_group
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_value_parameter, attributes, 1, $1);
    }
  ;

variable_parameter
  : _VAR parameter_group
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_variable_parameter, attributes, 1, $2);
    }
  ;

function_parameter
  : _FUNCTION parameter_group
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_function_parameter, attributes, 1, $2);
    }
  ;
   
procedure_parameter
  : _PROCEDURE idlist
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_procedure_parameter, attributes, 1, $2);
    }
  ;

parameter_group
  : idlist _COLON id
    {
      int i, len;

      /* Determine how many ids to process. */
      int num_ids = get_arity ($1);

      /* Store parameters in a global array params. */
      /* Put each id with its corresponding type in the symbol table. */
      for (i = 0;i < num_ids; i++)
      {
        /* Same as in variable_declaration- make it into a FUNCTION! */
        /* Get the name of the id from its attributes. */
	Node *id = get_child ($1, i);
	len = strlen (get_s_attribute (id->attributes, "name"));
	char *id_name = get_s_attribute (id->attributes, "name");

        /* Get the type of $3 from symbol table and associate it with id. */
        char *type_name = get_s_attribute ($3->attributes, "name");
        AttributeSet *set = symtab_get (stab, type_name);
	Type *type = get_p_attribute (set, "type");

        /* Place the name and type into the params array. */
	insert_param (id_name, type);
      }

      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_parameter_group, attributes, 2, $1, $3);
    }
  ;

label_declaration_part
  :
    { $$ = new_epsilon (); }
      
  | _LABEL unsigned_integer_list _SEMICOLON
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_label_declaration_part, attributes, 1, $2);
    }
  ;

constant_definition_part
  :
    { $$ = new_epsilon (); }
  |
    _CONST constant_definition_list 
    {  
     $$ = $2; }
  ;

constant_definition_list
  : constant_definition _SEMICOLON constant_definition_list_tail
    {
      printf ("in const def list\n");AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_constant_definition_list, attributes, cons ($1, $3));
    }
  ;

constant_definition_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | constant_definition _SEMICOLON constant_definition_list_tail
    { $$ = cons ($1, $3); }
  ;
type_definition_part
  :
    { $$ = new_epsilon (); }
  | _TYPE type_definition_list 
    { $$ = $2; }
  ;

type_definition_list
  : type_definition _SEMICOLON type_definition_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_type_definition_list, attributes, cons ($1, $3));
    }
  ;

type_definition_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | type_definition _SEMICOLON type_definition_list_tail
    { $$ = cons ($1, $3); }
  ;

variable_declaration_part
  :
    { $$ = new_epsilon (); }
  | _VAR variable_declaration_list
    { $$ = $2; }
  ;

variable_declaration_list
  : variable_declaration _SEMICOLON variable_declaration_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_variable_declaration_list, attributes, cons ($1, $3));
    }
  ;

variable_declaration_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | variable_declaration _SEMICOLON variable_declaration_list_tail
    { $$ = cons ($1, $3); }
  ;

procedure_declaration_list
  : /* empty */
    { $$ = new_epsilon (); }
  | procedure_or_function_declaration _SEMICOLON procedure_declaration_list_tail
    {
      AttributeSet *attributes = new_attribute_set (0);
      $$ = list2node (_procedure_declaration_list, attributes, cons ($1, $3));
    }
  ;

procedure_declaration_list_tail
  : /* epsilon */
    { $$ = new_epsilon (); }
  | procedure_or_function_declaration _SEMICOLON procedure_declaration_list_tail
    { $$ = cons ($1, $3); }
  ;

procedure_or_function_declaration
  : procedure_declaration
    { $$ = $1; }
  | function_declaration
    { $$ = $1; }
  ;

        /* 11. Function declarations */

function_declaration
  : function_heading 
    label_declaration_part
    constant_definition_part
    type_definition_part
    variable_declaration_part
    procedure_declaration_list
    compound_statement
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_function_declaration, attributes, 7,
                              $1, $2, $3, $4, $5, $6, $7);
    }
  ;

function_heading 
  : _FUNCTION id _LPAREN formal_parameters _RPAREN _COLON id _SEMICOLON
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_function_heading, attributes, 3, $2, $4, $7);
    }
  ;

        /* 12. Program s*/

program
  : program_heading 
    label_declaration_part
    constant_definition_part
    type_definition_part
    variable_declaration_part 
    procedure_declaration_list
    compound_statement
    _DOT
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_program, attributes, 7,
                              $1, $2, $3, $4, $5, $6, $7);
    }
  ;

program_heading 
  : /* epsilon */
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_program_heading, attributes, 0);
    }
  | _PROGRAM id _SEMICOLON
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_program_heading, attributes, 1, $2);
    }
  | _PROGRAM id _LPAREN idlist _RPAREN _SEMICOLON
    { 
      AttributeSet *attributes = new_attribute_set (0);
      $$ = new_interior_node (_program_heading, attributes, 2, $2, $4);
    }
;

%%

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
}
  
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
}

/* Our beautiful lexer. */
#include "lex.yy.c"

/* Code for handling attributes. */
#include "attribute.c"

/* Code for building parse trees. */
#include "parse-tree.c"
