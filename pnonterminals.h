#ifndef __PNONTERMINALS_H__
#define __PNONTERMINALS_H__

/**
 * pnonterminals.h
 *   Nonterminals for Pascal.  Whee.
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

  1. Since we've used an enumerated type for the tokens/terminals, we
     use an enumerated type to list the nonterminals.  We try to ensure
     that the range is different, so it's possible to tell them apart.

  2. The constants will be in all caps, which makes it a bit harder to
     distinguish from terminals.  To help, we also suffix nonterminals
     with underscores.  E.g, _EXP_.  (Recall that the underscores help
     avoid conflict with some standard names.)

 */


// +---------+--------------------------------------------------------
// | Headers |
// +---------+


// +-------+----------------------------------------------------------
// | Types |
// +-------+

/**
 * All of the interesting nonterminals in Pascal.
 */
typedef enum nonterminal
  {
    _NONTERMINALS_START = 2000,

    _EXP_,
    _TERM_,
    _FACTOR_,
    _MULOP_,
    _ADDOP_,
    
    _NONTERMINALS_END
  } nonterminal;

#endif // __PNONTERMINALS_H__
