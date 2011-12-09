
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

/* Line 189 of yacc.c  */
#line 39 "pascal.y"

#define YYSTYPE Node *

/* Line 189 of yacc.c  */
#line 127 "pascal.y"

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

 
/* Helper procedures for type checking. */

 
/* Returns the type of a terminal or nonterminal. */
TypeID
type (Node *node)
{
 Type *type_struct = get_p_attribute (node->attributes, "type");
 return type_struct->type;
}

/* Creates and returns a type struct. */
Type *
new_type_struct (TypeID type)
{
  struct Type *type_struct = malloc (sizeof (struct Type));
  type_struct->type = type; 
  return type_struct;
}

/* Checks if two variables have the same type. If so, return 1, otherwise
 * return 0.
 */
int
types_compatible (Node *child0, Node *child1)
{
  /* Return 1 if the children have the same type. */
  if (type (child0) == type (child1))
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
  printf ("hey arity %d\n", nn->arity);
  return nn->arity;
} // get_arity

// STUB
Type *get_params(Node *node)
{
 return 0;
}

 
/* Handling Parameters in procedures and functions. */


// Maximum number of parameters in a function or procedure
#define MAX_PARAMS 128

// Store parameters in a global array
Param params[MAX_PARAMS];

// Initially, there are no parameters
int params_size = 0;

void 
clear_params ()
{
 params_size = 0;
}

int
get_num_params ()
{
 return params_size;
}

// Store the next parameter in the array of params
void
insert_param (char *name, Type *type)
{
 params[params_size].name = name;
 params[params_size].type = type;
 params_size++;
}

/* Useful functions to avoid code duplication. */

/* Construct function and procedure type. */
Type *
function_procedure_type (Type *return_type, int num_params)
{

  Type *type_struct;
  
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
      
  /* Construct a type_struct for function or procedure */
  if (return_type ==NULL)
    type_struct = new_type_struct (TYPE_PROCEDURE);
  else
    type_struct = new_type_struct (TYPE_FUNCTION);
    
  type_struct->info.function_procedure = func_proc_struct;
  return type_struct;
}
  
/* Declare the symbol table for the program. */
SymTab *stab;


/* Line 189 of yacc.c  */
#line 661 "pascal.tab.c"

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
     _PROCEDURE = 289,
     _PROGRAM = 290,
     _RECORD = 291,
     _REPEAT = 292,
     _SET = 293,
     _THEN = 294,
     _TO = 295,
     _TRUE = 296,
     _TYPE = 297,
     _UNTIL = 298,
     _VAR = 299,
     _WHILE = 300,
     _WITH = 301,
     KEYWORDS_END = 302,
     PUNCTUATION_START = 303,
     _ASSIGN = 304,
     _COLON = 305,
     _COMMA = 306,
     _ELLIPSES = 307,
     _POINTER = 308,
     _SEMICOLON = 309,
     _LPAREN = 310,
     _RPAREN = 311,
     _LBRACKET = 312,
     _RBRACKET = 313,
     _DOT = 314,
     PUNCTUATION_END = 315,
     OPERATORS_START = 316,
     _NOT = 317,
     MULOPS_START = 318,
     _AND = 319,
     _DIV = 320,
     _MOD = 321,
     _SLASH = 322,
     _STAR = 323,
     MULOPS_END = 324,
     ADDOPS_START = 325,
     _DASH = 326,
     _OR = 327,
     _PLUS = 328,
     ADDOPS_END = 329,
     RELOPS_START = 330,
     _EQ = 331,
     _GE = 332,
     _GT = 333,
     _LE = 334,
     _LT = 335,
     _NE = 336,
     RELOPS_END = 337,
     OPERATORS_END = 338,
     TOKENS_END = 339
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
#line 787 "pascal.tab.c"

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
#define YYLAST   602

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  85
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  127
/* YYNRULES -- Number of rules.  */
#define YYNRULES  219
/* YYNRULES -- Number of states.  */
#define YYNSTATES  396

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   339

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
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84
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
     340,   342,   344,   346,   348,   350,   352,   356,   358,   363,
     366,   367,   369,   371,   373,   375,   377,   379,   381,   385,
     387,   389,   391,   393,   398,   405,   412,   418,   422,   425,
     426,   430,   432,   434,   436,   438,   440,   445,   450,   455,
     464,   473,   475,   477,   482,   487,   495,   502,   503,   505,
     507,   509,   511,   513,   516,   517,   521,   523,   526,   529,
     532,   536,   537,   541,   542,   545,   549,   550,   554,   555,
     558,   562,   563,   567,   568,   571,   575,   576,   580,   581,
     585,   586,   590,   592,   594,   602,   611,   620,   621,   625
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      86,     0,    -1,    -1,    87,   210,    -1,     9,    -1,    88,
      90,    -1,    -1,    51,    88,    90,    -1,    10,    -1,    88,
      -1,    91,    -1,    73,    -1,    71,    -1,    11,    -1,     6,
      -1,    92,    -1,    94,    -1,    31,    -1,    41,    -1,    23,
      -1,    93,    92,    -1,    93,    95,    -1,    92,    -1,    97,
      -1,    95,    -1,    98,    -1,    94,    -1,    99,   101,    -1,
      -1,    51,    99,   101,    -1,    95,    76,    99,    -1,   105,
      -1,   110,    -1,   127,    -1,    95,    76,   103,    -1,   106,
      -1,   109,    -1,    95,    -1,    55,   107,    56,    -1,    95,
     108,    -1,    -1,    51,    95,   108,    -1,    99,    52,    99,
      -1,   112,    -1,   111,    -1,    33,   112,    -1,   113,    -1,
     116,    -1,   125,    -1,   126,    -1,    14,    57,   114,    58,
      32,   103,    -1,   105,   115,    -1,    -1,    51,   105,   115,
      -1,    36,   117,    21,    -1,   118,    -1,   120,   119,    -1,
     121,    -1,    -1,    54,   118,    -1,   107,    50,   103,    -1,
      16,    95,    50,    95,    32,   123,    -1,   100,    50,    55,
     117,    56,    -1,   100,    -1,   122,   124,    -1,    -1,    54,
     122,   124,    -1,    38,    32,   105,    -1,    24,    32,   103,
      -1,    53,    95,    -1,   107,    50,   103,    -1,   132,    -1,
     133,    -1,   136,    -1,   129,   131,    -1,    -1,    51,   129,
     131,    -1,    95,    -1,   134,    -1,   135,    -1,   129,    57,
     142,    58,    -1,   129,    59,    95,    -1,   129,    53,    -1,
     129,    -1,    96,    -1,   147,    -1,   138,    -1,    55,   141,
      56,    -1,    62,   137,    -1,    57,   142,    58,    -1,    57,
      58,    -1,   139,   144,   137,    -1,   137,    -1,   140,   145,
     139,    -1,   145,   139,    -1,   139,    -1,   140,   146,   140,
      -1,   140,    -1,   141,   143,    -1,    -1,    51,   141,   143,
      -1,    68,    -1,    67,    -1,    65,    -1,    66,    -1,    64,
      -1,    73,    -1,    71,    -1,    72,    -1,    80,    -1,    79,
      -1,    76,    -1,    81,    -1,    77,    -1,    78,    -1,    29,
      -1,    95,    55,    56,    -1,    95,    55,   142,    56,    -1,
     150,   149,    -1,    -1,    54,   150,   149,    -1,   151,    -1,
     152,    -1,   153,    -1,   155,    -1,   154,    -1,   156,    -1,
      88,    50,   155,    -1,    88,    50,   156,    -1,   157,    -1,
     162,    -1,   163,    -1,   158,    -1,   159,    -1,   160,    -1,
     161,    -1,   129,    49,   141,    -1,    95,    -1,    95,    55,
     142,    56,    -1,    27,    88,    -1,    -1,   164,    -1,   165,
      -1,   174,    -1,   182,    -1,   166,    -1,   175,    -1,   183,
      -1,    15,   148,    21,    -1,   168,    -1,   170,    -1,   167,
      -1,   169,    -1,    28,   141,    39,   151,    -1,    28,   141,
      39,   151,    20,   151,    -1,    28,   141,    39,   151,    20,
     152,    -1,    16,   141,    32,   172,    21,    -1,   100,    50,
     150,    -1,   171,   173,    -1,    -1,    54,   171,   173,    -1,
     176,    -1,   178,    -1,   179,    -1,   177,    -1,   180,    -1,
      45,   141,    18,   151,    -1,    45,   141,    18,   152,    -1,
      37,   148,    43,   141,    -1,    25,    95,    49,   141,   181,
     141,    18,   151,    -1,    25,    95,    49,   141,   181,   141,
      18,   152,    -1,    40,    -1,    19,    -1,    46,   130,    18,
     151,    -1,    46,   130,    18,   152,    -1,   185,   195,   196,
     199,   202,   205,   164,    -1,    34,    95,    55,   186,    56,
      54,    -1,    -1,   188,    -1,   190,    -1,   191,    -1,   192,
      -1,   193,    -1,   187,   189,    -1,    -1,    54,   187,   189,
      -1,   194,    -1,    44,   194,    -1,    26,   194,    -1,    34,
     107,    -1,   107,    50,    95,    -1,    -1,    30,    89,    54,
      -1,    -1,    17,   197,    -1,   102,    54,   198,    -1,    -1,
     102,    54,   198,    -1,    -1,    42,   200,    -1,   104,    54,
     201,    -1,    -1,   104,    54,   201,    -1,    -1,    44,   203,
      -1,   128,    54,   204,    -1,    -1,   128,    54,   204,    -1,
      -1,   207,    54,   206,    -1,    -1,   207,    54,   206,    -1,
     184,    -1,   208,    -1,   209,   195,   196,   199,   202,   205,
     164,    -1,    26,    95,    55,   186,    56,    50,    95,    54,
      -1,   211,   195,   196,   199,   202,   205,   164,    59,    -1,
      -1,    35,    95,    54,    -1,    35,    95,    55,   107,    56,
      54,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   705,   705,   705,   734,   747,   756,   757,   764,   778,
     779,   783,   785,   790,   805,   822,   824,   826,   828,   830,
     835,   843,   851,   853,   855,   857,   859,   864,   873,   874,
     881,   899,   901,   903,   908,   928,   930,   932,   953,   961,
     971,   972,   981,  1034,  1036,  1041,  1049,  1051,  1053,  1055,
    1062,  1100,  1109,  1110,  1117,  1138,  1149,  1154,  1162,  1167,
    1172,  1180,  1188,  1193,  1201,  1210,  1211,  1218,  1228,  1238,
    1248,  1283,  1285,  1287,  1292,  1301,  1302,  1309,  1323,  1325,
    1332,  1342,  1357,  1367,  1369,  1371,  1373,  1375,  1377,  1385,
    1390,  1398,  1440,  1447,  1482,  1487,  1492,  1512,  1517,  1526,
    1527,  1538,  1543,  1548,  1553,  1558,  1568,  1573,  1578,  1588,
    1593,  1598,  1603,  1608,  1613,  1618,  1632,  1637,  1655,  1664,
    1665,  1670,  1672,  1677,  1679,  1684,  1686,  1691,  1699,  1707,
    1709,  1714,  1721,  1723,  1725,  1727,  1739,  1752,  1757,  1767,
    1778,  1787,  1789,  1791,  1793,  1798,  1800,  1802,  1809,  1816,
    1818,  1823,  1825,  1832,  1840,  1848,  1859,  1867,  1875,  1884,
    1885,  1892,  1894,  1896,  1901,  1903,  1910,  1918,  1928,  1938,
    1947,  1956,  1961,  1971,  1979,  1989,  2004,  2050,  2051,  2056,
    2058,  2060,  2062,  2067,  2076,  2077,  2082,  2090,  2098,  2106,
    2114,  2146,  2148,  2157,  2159,  2165,  2174,  2175,  2180,  2181,
    2186,  2195,  2196,  2202,  2203,  2209,  2218,  2219,  2225,  2226,
    2235,  2236,  2241,  2243,  2250,  2265,  2275,  2292,  2296,  2301
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
  "_LABEL", "_NIL", "_OF", "_PACKED", "_PROCEDURE", "_PROGRAM", "_RECORD",
  "_REPEAT", "_SET", "_THEN", "_TO", "_TRUE", "_TYPE", "_UNTIL", "_VAR",
  "_WHILE", "_WITH", "KEYWORDS_END", "PUNCTUATION_START", "_ASSIGN",
  "_COLON", "_COMMA", "_ELLIPSES", "_POINTER", "_SEMICOLON", "_LPAREN",
  "_RPAREN", "_LBRACKET", "_RBRACKET", "_DOT", "PUNCTUATION_END",
  "OPERATORS_START", "_NOT", "MULOPS_START", "_AND", "_DIV", "_MOD",
  "_SLASH", "_STAR", "MULOPS_END", "ADDOPS_START", "_DASH", "_OR", "_PLUS",
  "ADDOPS_END", "RELOPS_START", "_EQ", "_GE", "_GT", "_LE", "_LT", "_NE",
  "RELOPS_END", "OPERATORS_END", "TOKENS_END", "$accept", "start", "$@1",
  "unsigned_integer", "unsigned_integer_list",
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
  "simple_statement", "assignment_statement", "procedure_call",
  "goto_statement", "empty_statement", "safe_structured_statement",
  "unsafe_structured_statement", "compound_statement",
  "safe_conditional_statement", "unsafe_conditional_statement",
  "if_then_statement", "safe_if_then_else_statement",
  "unsafe_if_then_else_statement", "case_statement", "case_element",
  "case_element_list", "case_element_list_tail",
  "safe_repetitive_statement", "unsafe_repetitive_statement",
  "safe_while_statement", "unsafe_while_statement", "repeat_statement",
  "safe_for_statement", "unsafe_for_statement", "direction",
  "safe_with_statement", "unsafe_with_statement", "procedure_declaration",
  "procedure_heading", "formal_parameters", "formals", "formals_list",
  "formals_list_tail", "value_parameter", "variable_parameter",
  "function_parameter", "procedure_parameter", "parameter_group",
  "label_declaration_part", "constant_definition_part",
  "constant_definition_list", "constant_definition_list_tail",
  "type_definition_part", "type_definition_list",
  "type_definition_list_tail", "variable_declaration_part",
  "variable_declaration_list", "variable_declaration_list_tail",
  "procedure_declaration_list", "procedure_declaration_list_tail",
  "procedure_or_function_declaration", "function_declaration",
  "function_heading", "program", "program_heading", 0
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
     335,   336,   337,   338,   339
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    85,    87,    86,    88,    89,    90,    90,    91,    92,
      92,    93,    93,    94,    95,    96,    96,    96,    96,    96,
      97,    98,    99,    99,    99,    99,    99,   100,   101,   101,
     102,   103,   103,   103,   104,   105,   105,   105,   106,   107,
     108,   108,   109,   110,   110,   111,   112,   112,   112,   112,
     113,   114,   115,   115,   116,   117,   118,   118,   119,   119,
     120,   121,   122,   122,   123,   124,   124,   125,   126,   127,
     128,   129,   129,   129,   130,   131,   131,   132,   133,   133,
     134,   135,   136,   137,   137,   137,   137,   137,   137,   138,
     138,   139,   139,   140,   140,   140,   141,   141,   142,   143,
     143,   144,   144,   144,   144,   144,   145,   145,   145,   146,
     146,   146,   146,   146,   146,   146,   147,   147,   148,   149,
     149,   150,   150,   151,   151,   152,   152,   153,   154,   155,
     155,   156,   157,   157,   157,   157,   158,   159,   159,   160,
     161,   162,   162,   162,   162,   163,   163,   163,   164,   165,
     165,   166,   166,   167,   168,   169,   170,   171,   172,   173,
     173,   174,   174,   174,   175,   175,   176,   177,   178,   179,
     180,   181,   181,   182,   183,   184,   185,   186,   186,   187,
     187,   187,   187,   188,   189,   189,   190,   191,   192,   193,
     194,   195,   195,   196,   196,   197,   198,   198,   199,   199,
     200,   201,   201,   202,   202,   203,   204,   204,   205,   205,
     206,   206,   207,   207,   208,   209,   210,   211,   211,   211
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
       1,     1,     1,     1,     1,     1,     3,     1,     4,     2,
       0,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     4,     6,     6,     5,     3,     2,     0,
       3,     1,     1,     1,     1,     1,     4,     4,     4,     8,
       8,     1,     1,     4,     4,     7,     6,     0,     1,     1,
       1,     1,     1,     2,     0,     3,     1,     2,     2,     2,
       3,     0,     3,     0,     2,     3,     0,     3,     0,     2,
       3,     0,     3,     0,     2,     3,     0,     3,     0,     3,
       0,     3,     1,     1,     7,     8,     8,     0,     3,     6
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,   217,     1,     0,     3,   191,    14,     0,     0,
     193,   218,     0,     4,     6,     0,     0,   198,    40,     0,
       0,     5,   192,     0,     0,   194,     0,   203,     0,    39,
       0,     6,     0,   196,     0,     0,   199,     0,   208,    40,
     219,     7,     8,    13,    12,    11,     9,    10,    22,     0,
      26,    24,    23,    25,    30,     0,   195,     0,   201,     0,
       0,   204,     0,     0,   212,   191,     0,     0,   213,   191,
      41,    20,    21,   196,     0,     0,     0,     0,     0,     0,
       0,    37,     0,    34,    31,    35,    36,    32,    44,    43,
      46,    47,    48,    49,    33,     0,   200,     0,   206,     0,
       0,   193,   140,     0,   210,   193,   197,     0,     0,    45,
       0,     0,     0,    55,    58,    57,     0,    69,     0,     0,
     201,    70,     0,   205,   177,   177,   198,     0,     0,     0,
       0,   140,     0,     0,     0,    77,     0,    71,    72,    78,
      79,    73,     0,   119,   121,   122,   123,   125,   124,   126,
     129,   132,   133,   134,   135,   130,   131,   141,   142,   145,
     151,   149,   152,   150,   143,   146,   161,   164,   162,   163,
     165,   144,   147,   216,   209,     0,   198,    52,     0,    68,
       0,     0,    54,     0,    56,    67,    38,    42,   202,   206,
       0,     0,     0,     0,     0,   184,   178,   179,   180,   181,
     182,   186,     0,   203,    19,    17,    18,     0,     0,     0,
     107,   108,   106,    15,    16,    77,    84,    83,    92,    86,
      95,    97,     0,     0,    85,     0,   139,     0,     0,     0,
      77,    75,     0,   140,     0,     0,    82,     0,     0,   148,
     140,   118,   210,   203,     0,    51,     0,     0,    60,    59,
     207,   188,   189,   187,     0,     0,     0,   183,     0,   208,
       0,    90,    99,     0,    88,     0,   105,   103,   104,   102,
     101,     0,   115,   111,   113,   114,   110,   109,   112,     0,
       0,     0,    94,     0,   140,     0,   140,     0,    74,   140,
     127,   128,     0,   136,     0,    81,   119,   211,   208,    52,
       0,     0,   190,     0,   184,   176,     0,    87,     0,    98,
      89,   116,     0,    91,    93,    96,    28,     0,   159,     0,
       0,     0,     0,     0,     0,     0,   153,   168,   166,   167,
      75,   173,   174,   138,    80,   120,     0,    53,    50,     0,
       0,   185,   175,    99,   117,     0,    27,   140,     0,   158,
     156,   172,   171,     0,     0,     0,     0,     0,   140,   140,
      76,   214,    63,    65,    61,   215,   100,    28,   157,   159,
       0,     0,   140,   140,   140,   154,   155,     0,     0,    64,
      29,   160,   140,     0,     0,     0,    65,   169,   170,     0,
     140,     0,    66,     0,    62,   140
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    46,    15,    21,    47,   213,    49,   214,
     215,   216,    52,    53,    82,   317,   346,    55,    83,    95,
      84,    85,   193,    29,    86,    87,    88,    89,    90,   178,
     245,    91,   112,   113,   184,   114,   115,   363,   364,   379,
      92,    93,    94,   122,   217,   232,   288,   137,   138,   139,
     140,   141,   218,   219,   220,   221,   262,   263,   309,   271,
     223,   280,   224,   142,   241,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   318,   319,   349,   164,
     165,   166,   167,   168,   169,   170,   353,   171,   172,    64,
      65,   194,   195,   196,   257,   197,   198,   199,   200,   201,
      10,    17,    25,    56,    27,    36,    96,    38,    61,   123,
      66,   174,    67,    68,    69,     5,     6
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -322
static const yytype_int16 yypact[] =
{
    -322,    40,    12,  -322,    50,  -322,    38,  -322,    26,    65,
      68,  -322,    50,  -322,    45,    43,    50,    57,    58,    52,
      65,  -322,  -322,    46,    80,  -322,    50,    91,    50,  -322,
      83,    45,    55,    50,    73,    90,  -322,    50,   -13,    58,
    -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,   148,
    -322,  -322,  -322,  -322,  -322,    96,  -322,   194,    50,   110,
     115,  -322,    50,    50,  -322,    38,   161,   124,  -322,    38,
    -322,  -322,  -322,    50,   127,   167,   156,    14,   169,    50,
      50,   129,   141,  -322,  -322,  -322,  -322,  -322,  -322,  -322,
    -322,  -322,  -322,  -322,  -322,   152,  -322,   194,    50,   147,
     164,    68,   137,   157,   -13,    68,  -322,   162,   194,  -322,
      50,   173,   203,  -322,   171,  -322,   162,  -322,   170,    55,
      50,  -322,   174,  -322,    56,    56,    57,   506,    50,    65,
     506,   137,   506,    50,   181,   112,   -11,  -322,  -322,  -322,
    -322,  -322,   216,   185,  -322,  -322,  -322,  -322,  -322,  -322,
    -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,
    -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,  -322,
    -322,  -322,  -322,  -322,  -322,   188,    57,   197,   187,  -322,
     204,   194,  -322,    14,  -322,  -322,  -322,  -322,  -322,    50,
      50,    50,    50,   205,   201,   199,  -322,  -322,  -322,  -322,
    -322,  -322,   206,    91,  -322,  -322,  -322,   506,   418,   525,
    -322,  -322,  -322,  -322,  -322,   209,  -322,   -18,  -322,  -322,
     145,   250,   227,   525,  -322,   211,  -322,   229,   223,   251,
    -322,   138,   254,   420,   506,   506,  -322,   506,    50,  -322,
     137,  -322,   -13,    91,   162,  -322,   241,    50,  -322,  -322,
    -322,  -322,  -322,  -322,    50,   224,    56,  -322,   222,   -13,
     225,  -322,   233,   220,  -322,   487,  -322,  -322,  -322,  -322,
    -322,   525,  -322,  -322,  -322,  -322,  -322,  -322,  -322,   525,
     506,    55,   145,   506,   406,   506,   137,    50,  -322,   137,
    -322,  -322,   230,  -322,   231,  -322,   185,  -322,   -13,   197,
     194,   255,  -322,    50,   199,  -322,   161,  -322,   506,  -322,
    -322,  -322,   232,  -322,   145,   149,   239,   242,   237,   272,
      -8,    50,   506,   506,    50,   245,   277,  -322,  -322,  -322,
     138,  -322,  -322,  -322,  -322,  -322,   161,  -322,  -322,    55,
     247,  -322,  -322,   233,  -322,    55,  -322,   137,    55,  -322,
    -322,  -322,  -322,   506,   257,   264,   286,   289,   556,   137,
    -322,  -322,   258,   256,  -322,  -322,  -322,   239,  -322,   237,
     293,   506,   406,   406,   406,  -322,  -322,   259,    55,  -322,
    -322,  -322,   137,    -8,   292,    14,   256,  -322,  -322,   506,
     406,   260,  -322,   295,  -322,   406
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -322,  -322,  -322,    16,  -322,   288,  -322,    -6,  -322,   -30,
      -4,  -322,  -322,  -322,    17,  -321,   -43,   309,   -93,   306,
    -102,  -322,     7,   297,  -322,  -322,  -322,   261,  -322,  -322,
      35,  -322,   -45,   166,  -322,  -322,  -322,   -33,  -322,   -39,
    -322,  -322,  -322,   313,   113,    27,    22,  -322,  -322,  -322,
    -322,  -322,  -200,  -322,  -207,    77,   131,  -142,    15,  -322,
    -204,  -322,  -322,   228,    64,  -233,  -234,  -252,  -322,  -322,
    -227,   128,  -322,  -322,  -322,  -322,  -322,  -322,  -322,   -65,
    -322,  -322,  -322,  -322,  -322,  -322,    19,  -322,    -5,  -322,
    -322,  -322,  -322,  -322,  -322,  -322,   -12,  -322,  -322,  -322,
    -322,   248,   120,  -322,    75,  -322,  -322,  -322,  -322,   -73,
      94,   -22,  -322,   304,  -116,  -322,   262,  -180,  -322,   191,
    -228,   142,  -101,  -322,  -322,  -322,  -322
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -138
static const yytype_int16 yytable[] =
{
       8,   103,    50,   175,   121,   177,   290,   296,    18,   264,
     203,   351,    23,    62,   185,   179,   282,   279,   362,    19,
       7,    63,    34,   259,    39,    14,    48,    50,    51,    23,
     110,   306,   352,    18,   329,   236,    31,   332,   235,   237,
       3,   238,   236,    71,    59,    72,   237,     4,   238,    54,
     326,    48,   328,    81,    34,   331,     7,   362,    99,   100,
     243,     7,     7,   298,    13,    42,    43,    50,     9,    23,
     336,   313,   314,    18,    13,   117,    18,    50,    50,   126,
      11,    12,   190,   176,   111,    16,    50,   118,   248,    50,
     191,    48,   292,    81,    18,   294,    20,    22,   135,    26,
     192,    48,    48,    81,    81,    59,   180,   376,    30,    28,
      48,   279,    81,    48,   368,    51,    34,   251,   134,   253,
      18,    18,    32,   312,   225,   375,    44,   135,    45,   230,
     388,   290,  -137,  -137,    33,    37,   187,    40,   384,   328,
     331,   175,   299,     7,    58,   226,    13,   134,   387,    57,
      73,    50,   102,   127,     7,  -137,   375,    13,    42,   101,
      97,   387,   128,   105,   129,   130,  -137,   234,     7,    98,
      74,    13,    42,    43,   131,    48,   102,    81,   104,    18,
      75,   -24,   132,   133,   107,    18,    18,    18,    18,   287,
     111,   236,    77,   119,    78,   237,    59,   238,   252,   108,
       7,   116,   124,    13,    42,    43,   120,   338,    74,   266,
     267,   268,   269,   270,    50,   136,   173,    80,    75,   125,
     210,   211,   212,   181,   182,   183,   186,    76,   189,   135,
      77,   233,    78,    44,   295,    45,   135,   239,    48,   240,
      81,   342,   242,   301,   136,   246,   231,    79,   244,    80,
     302,    50,    18,   256,   247,   254,   134,   255,   222,   281,
     283,   227,   258,   229,   265,    44,   285,    45,   284,   286,
      50,   361,   289,   300,   303,    48,   305,    51,   310,   272,
     135,   307,   135,   230,   308,   135,   333,   339,   344,   334,
     345,   348,   347,   350,    48,   358,    81,   359,   316,   340,
     325,   365,   134,   372,   373,   134,   371,   374,   377,    50,
     378,   382,   390,   395,   385,    50,   394,   354,    50,    41,
     230,   210,   211,   212,   380,    24,   273,   274,   275,   276,
     277,   278,    35,    48,   337,    51,    70,   109,   260,    48,
     391,    51,    48,   135,    51,   386,   136,   392,    50,   249,
      60,   357,   360,   136,   135,   135,   316,   315,   366,   228,
     335,   291,   367,   134,   381,   316,   293,   369,   135,   135,
     135,   389,    48,   202,    51,   134,   304,   106,   135,   341,
     250,    18,   188,     0,   297,     0,   135,     0,   325,   325,
     325,   135,   111,     0,     0,   316,     0,   136,   134,   136,
     330,     0,   136,     0,     0,     0,   325,     0,     0,     0,
       0,   325,     7,     0,   320,    13,   327,     0,     0,     0,
       0,   102,   127,     0,     7,     0,     7,    13,    42,    43,
       0,   321,     0,   129,   322,   102,   127,   231,     0,   343,
       0,   204,     0,   131,     0,   128,     0,   129,   130,   205,
       0,   323,   324,   355,   356,     0,     0,   131,     0,   206,
     136,     0,     0,     0,     0,   132,   133,     0,     0,     0,
       0,   136,   136,   207,     0,   208,   261,     0,     0,     0,
     209,     0,     0,     0,   370,   136,   136,   136,     0,   210,
     211,   212,     0,     7,     0,   136,    13,    42,    43,     0,
       0,     0,   383,   136,     0,     0,     0,     0,   136,     0,
     204,     0,     7,     0,     0,    13,    42,    43,   205,     0,
     393,     0,     0,     0,     0,     0,     0,     0,   206,   204,
       0,     7,     0,     0,    13,    42,    43,   205,     0,     0,
       0,     0,   207,   311,   208,     0,     0,   206,   204,   209,
       0,     0,     0,     0,     0,     0,   205,     0,   210,   211,
     212,   207,     7,   208,     0,     0,   206,     0,   209,     0,
       0,   102,   127,     0,     0,     0,     0,   210,   211,   212,
     207,   321,   208,   129,   322,     0,     0,   209,     0,     0,
       0,     0,     0,   131,     0,     0,     0,     0,     0,     0,
       0,   323,   324
};

static const yytype_int16 yycheck[] =
{
       4,    66,    32,   104,    97,   107,   233,   240,    12,   209,
     126,    19,    16,    26,   116,   108,   223,   221,   339,    12,
       6,    34,    26,   203,    28,     9,    32,    57,    32,    33,
      16,   259,    40,    37,   286,    53,    20,   289,    49,    57,
       0,    59,    53,    49,    37,    49,    57,    35,    59,    32,
     284,    57,   286,    57,    58,   289,     6,   378,    62,    63,
     176,     6,     6,   243,     9,    10,    11,    97,    30,    73,
     298,   271,   279,    77,     9,    79,    80,   107,   108,   101,
      54,    55,    26,   105,    77,    17,   116,    80,   181,   119,
      34,    97,   234,    97,    98,   237,    51,    54,   102,    42,
      44,   107,   108,   107,   108,    98,   110,   359,    56,    51,
     116,   315,   116,   119,   347,   119,   120,   190,   102,   192,
     124,   125,    76,   265,   128,   359,    71,   131,    73,   133,
     382,   358,    20,    21,    54,    44,   119,    54,   372,   373,
     374,   242,   244,     6,    54,   129,     9,   131,   382,    76,
      54,   181,    15,    16,     6,    43,   390,     9,    10,    65,
      50,   395,    25,    69,    27,    28,    54,    55,     6,    54,
      14,     9,    10,    11,    37,   181,    15,   181,    54,   183,
      24,    52,    45,    46,    57,   189,   190,   191,   192,    51,
     183,    53,    36,    52,    38,    57,   189,    59,   191,    32,
       6,    32,    55,     9,    10,    11,    54,   300,    14,    64,
      65,    66,    67,    68,   244,   102,    59,    55,    24,    55,
      71,    72,    73,    50,    21,    54,    56,    33,    54,   233,
      36,    50,    38,    71,   238,    73,   240,    21,   244,    54,
     244,   306,    54,   247,   131,    58,   133,    53,    51,    55,
     254,   281,   256,    54,    50,    50,   240,    56,   127,    32,
      49,   130,    56,   132,    55,    71,    43,    73,    39,    18,
     300,   336,    18,    32,    50,   281,    54,   281,    58,    29,
     284,    56,   286,   287,    51,   289,    56,    32,    56,    58,
      51,    54,    50,    21,   300,    50,   300,    20,   281,   303,
     284,    54,   286,    39,    18,   289,    49,    18,    50,   339,
      54,    18,    20,    18,    55,   345,    56,   321,   348,    31,
     324,    71,    72,    73,   367,    16,    76,    77,    78,    79,
      80,    81,    26,   339,   299,   339,    39,    76,   207,   345,
     385,   345,   348,   347,   348,   378,   233,   386,   378,   183,
      37,   324,   330,   240,   358,   359,   339,   280,   343,   131,
     296,   233,   345,   347,   369,   348,   235,   348,   372,   373,
     374,   383,   378,   125,   378,   359,   256,    73,   382,   304,
     189,   385,   120,    -1,   242,    -1,   390,    -1,   372,   373,
     374,   395,   385,    -1,    -1,   378,    -1,   284,   382,   286,
     287,    -1,   289,    -1,    -1,    -1,   390,    -1,    -1,    -1,
      -1,   395,     6,    -1,   283,     9,   285,    -1,    -1,    -1,
      -1,    15,    16,    -1,     6,    -1,     6,     9,    10,    11,
      -1,    25,    -1,    27,    28,    15,    16,   324,    -1,   308,
      -1,    23,    -1,    37,    -1,    25,    -1,    27,    28,    31,
      -1,    45,    46,   322,   323,    -1,    -1,    37,    -1,    41,
     347,    -1,    -1,    -1,    -1,    45,    46,    -1,    -1,    -1,
      -1,   358,   359,    55,    -1,    57,    58,    -1,    -1,    -1,
      62,    -1,    -1,    -1,   353,   372,   373,   374,    -1,    71,
      72,    73,    -1,     6,    -1,   382,     9,    10,    11,    -1,
      -1,    -1,   371,   390,    -1,    -1,    -1,    -1,   395,    -1,
      23,    -1,     6,    -1,    -1,     9,    10,    11,    31,    -1,
     389,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    41,    23,
      -1,     6,    -1,    -1,     9,    10,    11,    31,    -1,    -1,
      -1,    -1,    55,    56,    57,    -1,    -1,    41,    23,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    -1,    71,    72,
      73,    55,     6,    57,    -1,    -1,    41,    -1,    62,    -1,
      -1,    15,    16,    -1,    -1,    -1,    -1,    71,    72,    73,
      55,    25,    57,    27,    28,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    37,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    45,    46
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    86,    87,     0,    35,   210,   211,     6,    95,    30,
     195,    54,    55,     9,    88,    89,    17,   196,    95,   107,
      51,    90,    54,    95,   102,   197,    42,   199,    51,   108,
      56,    88,    76,    54,    95,   104,   200,    44,   202,    95,
      54,    90,    10,    11,    71,    73,    88,    91,    92,    93,
      94,    95,    97,    98,    99,   102,   198,    76,    54,   107,
     128,   203,    26,    34,   184,   185,   205,   207,   208,   209,
     108,    92,    95,    54,    14,    24,    33,    36,    38,    53,
      55,    95,    99,   103,   105,   106,   109,   110,   111,   112,
     113,   116,   125,   126,   127,   104,   201,    50,    54,    95,
      95,   195,    15,   164,    54,   195,   198,    57,    32,   112,
      16,   107,   117,   118,   120,   121,    32,    95,   107,    52,
      54,   103,   128,   204,    55,    55,   196,    16,    25,    27,
      28,    37,    45,    46,    88,    95,   129,   132,   133,   134,
     135,   136,   148,   150,   151,   152,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     167,   168,   169,   170,   174,   175,   176,   177,   178,   179,
     180,   182,   183,    59,   206,   207,   196,   105,   114,   103,
      95,    50,    21,    54,   119,   105,    56,    99,   201,    54,
      26,    34,    44,   107,   186,   187,   188,   190,   191,   192,
     193,   194,   186,   199,    23,    31,    41,    55,    57,    62,
      71,    72,    73,    92,    94,    95,    96,   129,   137,   138,
     139,   140,   141,   145,   147,    95,    88,   141,   148,   141,
      95,   129,   130,    50,    55,    49,    53,    57,    59,    21,
      54,   149,    54,   199,    51,   115,    58,    50,   103,   118,
     204,   194,   107,   194,    50,    56,    54,   189,    56,   202,
     141,    58,   141,   142,   137,    55,    64,    65,    66,    67,
      68,   144,    29,    76,    77,    78,    79,    80,    81,   145,
     146,    32,   139,    49,    39,    43,    18,    51,   131,    18,
     155,   156,   142,   141,   142,    95,   150,   206,   202,   105,
      32,    95,    95,    50,   187,    54,   205,    56,    51,   143,
      58,    56,   142,   137,   139,   140,    99,   100,   171,   172,
     141,    25,    28,    45,    46,    88,   151,   141,   151,   152,
     129,   151,   152,    56,    58,   149,   205,   115,   103,    32,
      95,   189,   164,   141,    56,    51,   101,    50,    54,   173,
      21,    19,    40,   181,    95,   141,   141,   130,    50,    20,
     131,   164,   100,   122,   123,    54,   143,    99,   150,   171,
     141,    49,    39,    18,    18,   151,   152,    50,    54,   124,
     101,   173,    18,   141,   151,    55,   122,   151,   152,   181,
      20,   117,   124,   141,    56,    18
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
#line 705 "pascal.y"
    { 
      /* Create a new symbol table. */
      stab = symtab_new (); 
      int i;
      /* Put all basic types in the symbol table. */
      char *type_names[]  = {"boolean", "integer", "real", "char", "string"};
      TypeID types[] = {TYPE_BOOLEAN, TYPE_INTEGER, TYPE_REAL, TYPE_CHAR,
TYPE_STRING};
      for (i = 0; i < 5; i++)
      {
        Type *type_struct = new_type_struct (types[i]);
        AttributeSet *type_attributes = new_attribute_set (1);
        set_p_attribute (type_attributes, "type", type_struct);
        symtab_put (stab, type_names[i], type_attributes);
      } 

      /* An array to store procedure/function parameters. */
    ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 724 "pascal.y"
    { tree = (yyvsp[(2) - (2)]); ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 735 "pascal.y"
    {
      /* Set the type of the integer constant. */
      AttributeSet *attributes = new_attribute_set (2);
      Type *type_struct = new_type_struct (TYPE_INTEGER);
      set_p_attribute (attributes, "type", type_struct);
      set_i_attribute (attributes, "ivalue", atoi (yytext));
      Node *node = new_tnode (_INTEGER, attributes);
      (yyval) = node;
    ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 748 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_unsigned_integer_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 756 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 758 "pascal.y"
    {
      (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 765 "pascal.y"
    {
      /* Set the type and value of the real constant. */
      AttributeSet *attributes = new_attribute_set (2);
      struct Type *type_struct = malloc (sizeof (struct Type));
      type_struct->type = TYPE_REAL;
      set_p_attribute (attributes, "type", type_struct);
      set_r_attribute (attributes, "rvalue", atof (yytext));
      Node *node = new_tnode (_REAL, attributes);
      (yyval) = node;
    ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 784 "pascal.y"
    { (yyval) = new_tnode (_PLUS, new_attribute_set (0)); ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 786 "pascal.y"
    { (yyval) = new_tnode (_DASH, new_attribute_set (0)); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 791 "pascal.y"
    {
      /* Set the type of the string constant. */
      AttributeSet *attributes = new_attribute_set (2);
      Type *type_struct = new_type_struct (TYPE_STRING);
      set_p_attribute (attributes, "type", type_struct);
      set_s_attribute (attributes, "svalue", strdup (yytext));
      Node *node = new_tnode (_STRING, attributes);
      (yyval) = node;
    ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 806 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_s_attribute (attributes, "name", strdup (yytext));
      Node *node = new_tnode (_IDENTIFIER, attributes);
      (yyval) = node;
    ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 823 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 825 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 827 "pascal.y"
    { (yyval) = new_tnode (_NIL, new_attribute_set (0)); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 829 "pascal.y"
    { (yyval) = new_tnode (_TRUE, new_attribute_set (0)); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 831 "pascal.y"
    { (yyval) = new_tnode (_FALSE, new_attribute_set (0)); ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 836 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_signed_number, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 844 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_signed_identifier, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 852 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 854 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 856 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 858 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 860 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 865 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_constant_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 873 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 875 "pascal.y"
    {
      (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 882 "pascal.y"
    {
      
      /* Get the name of the id. */
      //char *id_name = get_s_attribute ($1->attributes, "name");

      /* Put the new constant in the symbol table. */
      AttributeSet *attributes = new_attribute_set (1);
   
      /* Retrieve the value of the constant. */

      (yyval) = new_interior_node (_constant_definition, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 900 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 902 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 904 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 909 "pascal.y"
    {
      /* Get the name of the id. */
      char *id_name = get_s_attribute ((yyvsp[(1) - (3)])->attributes, "name");

      /* Put the new type in the symbol table. */
      TypeID alias_type = type ((yyvsp[(3) - (3)]));
      Type *type_struct = new_type_struct (alias_type);
      AttributeSet *type_attributes = new_attribute_set (1);
      set_p_attribute (type_attributes, "type", type_struct);
      symtab_put (stab, id_name, type_attributes);
      
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_type_definition, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 929 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 931 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 933 "pascal.y"
    { 
      /* Get the name of the id (new type) from its attributes. */
      char *id_name = get_s_attribute ((yyvsp[(1) - (1)])->attributes, "name");

      /* Create a new type struct. */
      struct Type *type_struct = malloc (sizeof (struct Type));
      AttributeSet *id_attributes = symtab_get (stab, id_name);
      type_struct = get_p_attribute (id_attributes, "type");
      if (type_struct == NULL)
        fprintf (stderr, "Not a valid type name.\n");

      AttributeSet *attributes = new_attribute_set (1);
      set_p_attribute (attributes, "type", type_struct);
      (yyval) = new_interior_node (_simple_type, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 954 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_scalar_type, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 962 "pascal.y"
    { 
      /* Associate a type with each id in idlist. */ 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_idlist, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 971 "pascal.y"
    { printf ("epsilon in tail\n"); (yyval) = new_epsilon (); ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 973 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 982 "pascal.y"
    {
      /* Create a type_struct for the subrange type. */
      struct SubrangeType *subrange_struct = 
                                   malloc (sizeof (struct SubrangeType));

      
      /* If bounds are incompatible, then throw an appropriate error. */
      if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
        fprintf (stderr, "Incompatible types in subrange!\n");

      printf ("in subrange\n");
      /* Types are compatible, so assign the type to subrange. */
      Type *const_type_struct = get_p_attribute ((yyvsp[(1) - (3)])->attributes, "type");
      subrange_struct->type = const_type_struct;

      /* Retrieve the constant values based on the type. */
      Attribute lower;
      Attribute upper;

      switch (const_type_struct->type)
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
      Type *type_struct = new_type_struct (TYPE_SUBRANGE);
      type_struct->info.subrange = subrange_struct;
      set_p_attribute (subrange_attributes, "type", type_struct);
      (yyval) = new_interior_node (_subrange_type, subrange_attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 1035 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 1037 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 1042 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_packed_structured_type, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 1050 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1052 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1054 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 1056 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 1063 "pascal.y"
    {
      int i;

      /* Create a type_struct for the array type. */
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
      Type *type_struct = new_type_struct (TYPE_ARRAY);
      type_struct->info.array = array_struct;
      set_p_attribute (array_attributes, "type", type_struct);
      (yyval) = new_interior_node (_array_type, array_attributes, 2, (yyvsp[(3) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 1101 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_simple_type_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 1109 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 1111 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 1118 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_record_type, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 1139 "pascal.y"
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
#line 1150 "pascal.y"
    {
      (yyval) = (yyvsp[(2) - (2)]);
      set_child ((yyval), 0, cons ((yyvsp[(1) - (2)]), get_child ((yyval), 0)));
    ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 1155 "pascal.y"
    { 
      (yyval) = new_interior_node (_field_list, NULL, 2, new_epsilon (), (yyvsp[(1) - (1)]));
    ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 1162 "pascal.y"
    { 
      Node *left = new_epsilon ();
      Node *right = new_epsilon ();
      (yyval) = new_interior_node (_field_list, NULL, 2, left, right);
    ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 1168 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 1173 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_record_section, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1181 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant_part, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 1189 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant, attributes, 2, (yyvsp[(1) - (5)]), (yyvsp[(4) - (5)]));
    ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 1194 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variant, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1202 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variant_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 1210 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 1212 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 1219 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_set_type, attributes, 1, (yyvsp[(3) - (3)]));
    ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 1229 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_file_type, attributes, 1, (yyvsp[(3) - (3)]));
    ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 1239 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_pointer_type, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 1249 "pascal.y"
    {
      int i;
      /* Determine how many ids to process. */
      int num_ids = get_arity ((yyvsp[(1) - (3)]));
      
      printf ("ids: %d\n", num_ids);
      /* Put each id with its corresponding type in the symbol table. */
      for (i = 0; i < num_ids; i++)
      {                                                                              
        /* Get the name of the id from its attributes. */
        Node *id = get_child ((yyvsp[(1) - (3)]), i);
        char *id_name = get_s_attribute (id->attributes, "name");
        
        /* Set the type. */
        AttributeSet *sym_attributes = new_attribute_set (1);
        Type *type_struct = new_type_struct (type ((yyvsp[(3) - (3)])));
        set_p_attribute (sym_attributes, "type", type_struct);

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
#line 1284 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 1286 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 1288 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 1293 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variable_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1301 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 1303 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 1310 "pascal.y"
    {
      char *id_name = get_s_attribute ((yyvsp[(1) - (1)])->attributes, "name");
      AttributeSet *set = symtab_get (stab, id_name);
      Type *type = get_p_attribute (set, "type");
      AttributeSet *attributes = new_attribute_set (1);
      set_p_attribute (attributes, "type", type);
      (yyval) = new_interior_node (_entire_variable, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 1324 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1326 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 1333 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_indexed_variable, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1343 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_field_designator, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 1358 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_referenced_variable, attributes, 1, (yyvsp[(1) - (2)]));
    ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1368 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1370 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1372 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1374 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1376 "pascal.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1378 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_negate, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 1386 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_set, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1391 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_empty_set, attributes, 0);
    ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1399 "pascal.y"
    {
      /* Get the mulop operator. */
      int operator = get_operator ((yyvsp[(2) - (3)]));

      /* Determine which operation we are performing and thus determine
       * allowed types.  
       * If types are inappropriate, throw an appropriate error.
       */
      switch (operator)
      {
       case (_STAR): 
       case (_SLASH):
       case (_DIV):
         if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (type ((yyvsp[(1) - (3)])) != TYPE_INTEGER || type((yyvsp[(1) - (3)])) != TYPE_REAL)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
        break;
       case (_MOD):
         if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (type ((yyvsp[(1) - (3)])) != TYPE_INTEGER)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
        break;
       case (_AND):
         if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
           fprintf (stderr, "Incompatible types in expression!\n");
         if (type ((yyvsp[(1) - (3)])) != TYPE_BOOLEAN)
           fprintf (stderr, "Operands have incorrect type in expression.\n");
       }

      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (1);
      Type *type_struct = new_type_struct (type ((yyvsp[(3) - (3)])));
      set_p_attribute (attributes, "type", type_struct);

      /* We do not do operand conversion in this implementation, so just pass
       * the children.
       */
      (yyval) = new_interior_node (_term, attributes, 3, (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1441 "pascal.y"
    {
      (yyval) = (yyvsp[(1) - (1)]);
    ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1448 "pascal.y"
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
        if (type ((yyvsp[(1) - (3)])) != TYPE_INTEGER && type((yyvsp[(1) - (3)])) != TYPE_REAL)
          fprintf (stderr, "Operands have incorrect type in expression.\n");
      }
      /* Otherwise, the operator is OR. */
      else 
      {
        if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
          fprintf (stderr, "Incompatible types in expression!\n");
        if (type ((yyvsp[(1) - (3)])) != TYPE_BOOLEAN)
          fprintf (stderr, "Operands have incorrect type in expression.\n");
      }
 
      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (1);
      Type *type_struct = new_type_struct (type ((yyvsp[(3) - (3)])));
      set_p_attribute (attributes, "type", type_struct);

      /* We do not do operand conversion in this implementation, so just pass
       * the children.
       */
      (yyval) = new_interior_node (_simple, attributes, 3, (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1483 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_simple, attributes, 2, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
    ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1488 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1493 "pascal.y"
    {
      /* If types are incompatible or inappropriate, throw an appropriate 
       * error. 
       */
      if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
        fprintf (stderr,"Incompatible types in expression!\n"); 
      if (type ((yyvsp[(1) - (3)])) != TYPE_INTEGER || type((yyvsp[(1) - (3)])) != TYPE_REAL)
        fprintf (stderr, "Operands have incorrect type in expression.\n");

      /* Otherwise the types are compatible, so construct the node. */
      AttributeSet *attributes = new_attribute_set (1);
      Type *type_struct = new_type_struct (type ((yyvsp[(3) - (3)])));
      set_p_attribute (attributes, "type", type_struct);

      /* We do not do operand conversion in this implementation, so just pass
       * the children.
       */
      (yyval) = new_interior_node (_expr, attributes, 3, (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1513 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1518 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_expr_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1526 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1528 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1539 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _STAR, attributes);
    ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1544 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _SLASH, attributes);
    ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1549 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _DIV, attributes);
    ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1554 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _MOD, attributes);
    ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1559 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_mulop, _AND, attributes);
    ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1569 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _PLUS, attributes);
    ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1574 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _DASH, attributes);
    ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1579 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_addop, _OR, attributes);
    ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 1589 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _LT, attributes);
    ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 1594 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _LE, attributes);
    ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 1599 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _EQ, attributes);
    ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1604 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _NE, attributes);
    ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 1609 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _GE, attributes);
    ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 1614 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _GT, attributes);
    ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 1619 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_relop, _IN, attributes);
    ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1633 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_call, attributes, 1, (yyvsp[(1) - (3)]));
    ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1638 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_call, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1656 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_statement_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1664 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1666 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1671 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1673 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1678 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1680 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1685 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1687 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1692 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_labeled_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1700 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_labeled_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1708 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1710 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1715 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 1722 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 1724 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1726 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1728 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1740 "pascal.y"
    {
      if (types_compatible ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])) == 0)
        fprintf (stderr, "Expression has incompatible type in assignment!\n");
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_assignment_statement, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1753 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_call, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1758 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_call, attributes, 2, (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
    ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1768 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_goto_statement, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1778 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_empty_statement, attributes, 0);
    ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 1788 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 1790 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 1792 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 1794 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 1799 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 1801 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 1803 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 1810 "pascal.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 1817 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 1819 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 1824 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 1826 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 1833 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 1841 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_else_statement, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 1849 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_if_then_else_statement, attributes, 3, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(6) - (6)]));
    ;}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 1860 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_case_statement, attributes, 2, (yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]));
    ;}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 1868 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_case_element, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 1876 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_case_element_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 1884 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 1886 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 1893 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 1895 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 1897 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 1902 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 1904 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 1911 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_while_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 1919 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_while_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 1929 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 1939 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_for_statement, attributes, 5, 
                              (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(8) - (8)]));
    ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 1948 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_for_statement, attributes, 5, 
                              (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(8) - (8)]));
    ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 1957 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_direction, _TO, attributes);
    ;}
    break;

  case 172:

/* Line 1455 of yacc.c  */
#line 1962 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = simple_unary_tree (_direction, _DOWNTO, attributes);
    ;}
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 1972 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 174:

/* Line 1455 of yacc.c  */
#line 1980 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_repeat_statement, attributes, 2, (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
    ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 1996 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_declaration, attributes, 7,
                              (yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
    ;}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 2005 "pascal.y"
    {
      Param param;
      int i;
            
      /* Get the name of the id from its attributes. */ 
      char *id_name = get_s_attribute ((yyvsp[(2) - (6)])->attributes, "name");

      /* Construct the procedure type. */
      int num_params = get_num_params ();
      Type *type_struct = function_procedure_type (NULL, num_params);

      /* Store info about procedure's params. */
      AttributeSet *proc_attributes = new_attribute_set (1);
      set_p_attribute (proc_attributes, "type", type_struct);

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
          param = type_struct->info.function_procedure->params[i];
          AttributeSet *param_attributes = new_attribute_set (1);
          set_p_attribute (param_attributes, "type", param.type);
          symtab_put (stab, param.name, param_attributes);
        }
      }

      /* Associate attributes with the node. */
      AttributeSet *node_attributes = new_attribute_set (1);
      set_p_attribute (node_attributes, "type", type_struct);
      (yyval) = new_interior_node (_procedure_heading, node_attributes, 2, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]));
    ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 2050 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 2052 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 2057 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 2059 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 2061 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 2063 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 2068 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_formals_list, attributes, cons ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])));
    ;}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 2076 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 2078 "pascal.y"
    { (yyval) = cons ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 2083 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_value_parameter, attributes, 1, (yyvsp[(1) - (1)]));
    ;}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 2091 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_variable_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 2099 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 2107 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_procedure_parameter, attributes, 1, (yyvsp[(2) - (2)]));
    ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 2115 "pascal.y"
    {
      int i;
      /* Determine how many ids to process. */
      int num_ids = get_arity ((yyvsp[(1) - (3)]));

      /* Clear the array of params. */
      clear_params();

      /* Store parameters in a global array params. */
      /* Put each id with its corresponding type in the symbol table. */
      for (i = 0;i < num_ids; i++)
      {
        /* Same as in variable_declaration- make it into a FUNCTION! */
        /* Get the name of the id from its attributes. */
	Node *id = get_child ((yyvsp[(1) - (3)]), i);
	char *id_name = get_s_attribute (id->attributes, "name");

        /* Get the type of $3 from symbol table and associate it with id. */
        Type *type_struct = new_type_struct (type ((yyvsp[(3) - (3)])));

        /* Place the name and type into the params array. */
	insert_param (id_name, type_struct);
      }

      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_parameter_group, attributes, 2, (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
    ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 2146 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 2149 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_label_declaration_part, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 2157 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 2160 "pascal.y"
    {  
     (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 2166 "pascal.y"
    {
      printf ("in const def list\n");AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_constant_definition_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 2174 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 2176 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 2180 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 2182 "pascal.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 2187 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_type_definition_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 2195 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 2197 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 2202 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 2204 "pascal.y"
    { printf ("_VAR\n");
      (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 2210 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_variable_declaration_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 2218 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 2220 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 2225 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 2227 "pascal.y"
    {
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = list2node (_procedure_declaration_list, attributes, cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])));
    ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 2235 "pascal.y"
    { (yyval) = new_epsilon (); ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 2237 "pascal.y"
    { (yyval) = cons ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 2242 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 2244 "pascal.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 2257 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_declaration, attributes, 7,
                              (yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)]));
    ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 2266 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_function_heading, attributes, 3, (yyvsp[(2) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(7) - (8)]));
    ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 2283 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program, attributes, 7,
                              (yyvsp[(1) - (8)]), (yyvsp[(2) - (8)]), (yyvsp[(3) - (8)]), (yyvsp[(4) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(6) - (8)]), (yyvsp[(7) - (8)]));
    ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 2292 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 0);
    ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 2297 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 1, (yyvsp[(2) - (3)]));
    ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 2302 "pascal.y"
    { 
      AttributeSet *attributes = new_attribute_set (0);
      (yyval) = new_interior_node (_program_heading, attributes, 2, (yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]));
    ;}
    break;



/* Line 1455 of yacc.c  */
#line 4590 "pascal.tab.c"
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
#line 2308 "pascal.y"


/* Our beautiful lexer. */
#include "lex.yy.c"

/* Code for handling attributes. */
#include "attribute.c"

/* Code for building parse trees. */
#include "parse-tree.c"

