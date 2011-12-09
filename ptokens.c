#ifndef __PTOKENS_H__
#define __PTOKENS_H__

/**
 * ptokens.h
 *   Information on Pascal tokens for our lexical analyzer.
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
  1. This file primarily provides an enumerated type for token types.  The
     types follow their natural names, and are prefixed with an underscore.
     The underscore helps to avoid naming conflicts with some standard C
     constants, but keeps things nice and concise.

  2. I've tried to make it easier to tell the more general type of
     a token by choosing specific ranges of numbers for different
     kinds of token types: E.g., constants are in the 200's, operators,
     are in the 300's, etc.  I call the grouping of types 'metatypes'.

     METATYPE_START and METATYPE_END give you the beginning and ending of
     a range of token types.  You can figure out how many tokens have a
     particular metatype by subtracting the beginning from the end, and
     then subtracting another 1.  E.g.,
   
       int num_keywords = KEYWORDS_END - KEYWORDS_START - 1;
   
     or
   
       if ((PUNCTUATION_START < toktype) && (toktype < PUNCTUATION_END))
         deal_with_punctuation (toktype, yytext);

  3. One design question for the lexer is whether to start grouping
     some related things under a single token heading.  E.g., should we
     call *, /, div, and mod all _MULOPs or not.  Since some of those
     symbols serve multiple purposes (in particular, = can be a comparison
     operator or just an equals sign for constant declarations), I've
     decided not to create operator types, but I've created metatypes for
     the various classes of operator (MULOP, ADDOP, RELOP).
 */

  
// +-------+----------------------------------------------------------
// | Types |
// +-------+

typedef enum toktype
  {
    // Special tokens
    _ERROR = -1,        // Some random error
    _EOF = 0,           // End of file.  f?lex returns 0 on eof, so
                        // this is useful.

    // General tokens: 100 range
    GENERAL_START = 100,
    _IDENTIFIER,
    GENERAL_END,    

    // Constants: 200 range
    CONSTANTS_START = 200,
    _INTEGER,
    _REAL,          
    _STRING,       
    CONSTANTS_END,

    // Keywords: 300 range
    KEYWORDS_START = 300,
    _ARRAY,
    _BEGIN,
    _CASE,
    _CONST,
    _DO,
    _DOWNTO,
    _ELSE,
    _END,
    _FILE,
    _FOR,
    _FUNCTION,
    _GOTO,
    _IF,
    _IN,
    _LABEL,
    _NIL,
    _OF,
    _PACKED,
    _PROCEDURE,
    _PROGRAM,
    _RECORD,
    _REPEAT,
    _SET,
    _THEN,
    _TO,
    _TYPE,
    _UNTIL,
    _VAR,
    _WHILE,
    _WITH,
    KEYWORDS_END,
   
    // Punctuation: 500 range
    PUNCTUATION_START = 500,
    _ASSIGN,            // :=
    _COLON,
    _COMMA,
    _ELLIPSES,          // ..
    _POINTER,           // ^, not to be confused with the "and" symbol
    _SEMICOLON,
    _LPAREN,
    _RPAREN,
    _LBRACKET,
    _RBRACKET,
    PUNCTUATION_END,

    // Operators (more or less): 1000 range
    OPERATORS_START = 1000,
    _NOT,               // Usually 'not', but maybe !

    MULOPS_START = 1100,
    _AND,               // Usually 'and', but maybe &
    _DIV,
    _MOD,
    _SLASH,		// /
    _STAR,		// *
    MULOPS_END,

    ADDOPS_START = 1200,
    _DASH,		// -
    _OR,                // Usually 'or', but maybe |
    _PLUS,              // +
    ADDOPS_END,

    RELOPS_START = 1300,
    _EQ,                // =
    _GE,                // >=
    _GT,                // >
    _LE,                // <=
    _LT,                // <
    _NE,                // <>
    RELOPS_END,
    OPERATORS_END,

  } toktype;


// +-----------+------------------------------------------------------
// | Functions |
// +-----------+

void print_token (toktype type);


#endif // __PTOKENS_H__
