/**
 * pascal.y
 *   A Pascal parser
 */

/* Headers that we rely upon. */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "attribute.h"
#include "parse-tree.h"
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
%token _PROCEDURE
%token _PROGRAM
%token _RECORD
%token _REPEAT
%token _SET
%token _THEN
%token _TO
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
    _assignment_statement,
    _compound_statement,
    _empty_statement,
    _expr,
    _factor,
    _id,
    _mulop,
    _statement,
    _statement_list,
    _term,
  } nonterms;

/* The names of the nonterminals. */
char *nonterm_names[] =
  {
    "addop",
    "assignment_statement",
    "compound_statement",
    "empty_statement",
    "expr",
    "factor",
    "id",
    "mulop",
    "statement",
    "statement_list",
    "term"
  };

/* The names of the tokens. */
char *toknames[] =
  {
    "<tokens>",
    "*error*",
    "<general>", "ID", "</general>",
    "<constants>", "INT", "REAL", "STRING", "</constants>",
    "<keywords>", "ARRAY", "BEGIN", "CASE", "CONST", "DO",
    "DOWNTO", "ELSE", "END", "FILE", "FOR", "FUNCTION", "GOTO",
    "IF", "IN", "LABEL", "NIL", "OF", "PACKED", "PROCEDURE",
    "PROGRAM", "RECORD", "REPEAT", "SET", "THEN", "TO", "TYPE",
    "UNTIL", "VAR", "WHILE", "WITH", "</keywords>",
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

/* Agh!  We need to declare yytext b/c we use it below. It comes
 * from f?lex.
 */
extern char *yytext;

/* Our lovely parse tree. */
Node *tree = NULL;

/* We seem to make a lot of simple_unary trees whose only child is
 * a leaf (see mulop and addop), so here's a helpful helper.
 */
static Node *
simple_unary_tree (int nonterm, int tok, AttributeSet *attributes)
{
  Node *node = new_nnode (nonterm, 1, attributes);
  set_child (node, 0, new_tnode (tok, new_attribute_set (0)));
  return node;
} // simple_unary_tree

%}

%%
start
  : expr
    { tree = $1; }
  ;

program
  : statement
    { $$ = $1; }
  ;

statement
  : assignment_statement
    { $$ = $1; }
  | compound_statement
    { $$ = $1; }
  | empty_statement
    { $$ = $1; }
  ;

assignment_statement
  : id _ASSIGN expr
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_assignment_statement, 2, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $3);
      $$ = node;
    }
  ;

compound_statement
  : _BEGIN statement_list _END
    { $$ = $2; }
  ;

empty_statement
  :
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_empty_statement, 0, attributes);
      $$ = node;
    }
  ;

statement_list
  : statement
    { $$ = $1; }
  | statement _SEMICOLON statement_list
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_statement_list, 2, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $3);
      $$ = node;
    }
  ;

expr
  : expr addop term
    {
      Node *node = new_nnode (_expr, 3, new_attribute_set (0));
      set_child (node, 0, $1);
      set_child (node, 1, $2);
      set_child (node, 2, $3);
      $$ = node;
    }
  | term
    {
      $$ = $1;
    }
  ;

term
  : term mulop factor
    {
      Node *node = new_nnode (_expr, 3, new_attribute_set (0));
      set_child (node, 0, $1);
      set_child (node, 1, $2);
      set_child (node, 2, $3);
      $$ = node;
    }
  | factor
    {
      $$ = $1;
    }
  ;

factor
  : _INTEGER
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_i_attribute (attributes, "value", atoi (yytext));
      Node *node = new_tnode (_INTEGER, attributes);
      $$ = node;
    }
  | id
    { $$ = $1; }
  | _LPAREN expr _RPAREN
    { $$ = $2; }
  ;

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
  ;

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
  ;

id
  : _IDENTIFIER
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_s_attribute (attributes, "name", strdup (yytext));
      Node *node = new_tnode (_IDENTIFIER, attributes);
      $$ = node;
    }
  ;

%%

/* Our beautiful lexer. */
#include "lex.yy.c"

/* Code for handling attributes. */
#include "attribute.c"

/* Code for building parse trees. */
#include "parse-tree.c"
