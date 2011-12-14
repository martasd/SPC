#ifndef __STAC_H__
#define __STAC_H__

/**
 * stac.h - A simple three-address code implementation.
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

// +---------+--------------------------------------------------------
// | Headers |
// +---------+

#include <stdio.h>	// For FILE


// +-------+----------------------------------------------------------
// | Types |
// +-------+

/**
 * Instruction codes.
 */
enum OpCode
  {
    // I like putting NOOP first
    NOOP,
    // Control
    JMP,
    JEQI,	// Integer
    JNEI,
    JGTI,
    JLEI,
    JEQF,	// Float
    JNEF,
    JGTF,
    JLEF,
    EXIT,
    // Data movement
    IMOV,
    FMOV,
    SMOV,
    CMOV,
    // Type conversion
    I2F,
    F2I,
    C2I,
    // Arithmetic
    IADD,
    ISUB,
    IMUL,
    IDIV,
    IMOD,
    FADD,
    FSUB,
    FMUL,
    FDIV,
    // Input
    IREAD,
    FREAD,
    SREAD,
    CREAD,
    // Output
    IWRITE,
    FWRITE,
    SWRITE,
    CWRITE,
    WRITELN,
    // Labels
    _LABL,
    // Hack.  A nice way to know how many operators there are.
    LAST_OP,
  };
typedef enum OpCode OpCode;

/**
 * The kinds of parameters.
 */
enum ParamType
  {
    NONE,
    ICONSTANT,
    FCONSTANT,
    SCONSTANT,
    CCONSTANT,
    REGISTER,
    ABSOLUTE,
    RELATIVE,
    LABEL,
  };
typedef enum ParamType ParamType;

/**
 * One of our many registers
 */
struct Register
  {
    char *name;		// The printable name
    int index;		// Internal use
  };
typedef struct Register Register;

/**
 * A relative address.
 */
struct RelAddr
  {
    Register reg;
    int offset;
  };
typedef struct RelAddr RelAddr;

/**
 * One of the many kinds of parameters.
 */
struct StacParameter
  {
    ParamType type;
    union
      {
        int i;          // Integer constant and absolute address
        float f;        // Real constant
        char *s;        // Label or string constant
        char c;         // Character constant
        Register r;	// A register
        RelAddr a;      // A relative address
      } info;
  };
typedef struct StacParameter StacParameter;

/**
 * An instruction.
 */
struct Instruction
  {
    OpCode op;
    StacParameter *params[3];
  };
typedef struct Instruction Instruction;


// +-----------+------------------------------------------------------
// | Constants |
// +-----------+

#define INT_SIZE sizeof(int)
#define REAL_SIZE sizeof(float)
#define CHAR_SIZE sizeof(char)
#define STRING_SIZE sizeof(char *)


// +-----------+------------------------------------------------------
// | Variables |
// +-----------+

extern StacParameter *SP;
extern StacParameter *BP;
extern StacParameter *HP;
extern StacParameter *R0;
extern StacParameter *PC;


// +---------------------------+--------------------------------------
// | Primitive Building Blocks |
// +---------------------------+

/**
 * Set up stac.  Must be called before any other stac procedures are
 * called.
 */
void stac_init (void);

/**
 * Fill in an instruction.
 */
void build_instruction (Instruction *instruction, OpCode op, 
                        StacParameter *p1, StacParameter *p2, StacParameter *p3);

/**
 * Get the register associated with a particular name.
 */
StacParameter *get_register (char *name);

/**
 * Create a new integer constant.
 */
StacParameter *new_iconstant (int val);

/**
 * Create a new floating point constant.
 */
StacParameter *new_fconstant (float val);

/**
 * Create a new character constant
 */
StacParameter *new_cconstant (char val);

/**
 * Create a new string constant.  Does not copy the string.
 */
StacParameter *new_sconstant (char *val);

/**
 * Create a new label.  Does not copy the string.
 */
StacParameter *new_label (char *name);

/**
 * Create a new absolute address.  Does not copy the string.
 */
StacParameter *new_absolute (int address);

/**
 * Create a new relative address.  
 */
StacParameter *new_relative (Register reg, int offset);


// +-----------+------------------------------------------------------
// | Execution |
// +-----------+

/**
 * Run a program, starting with instruction 0, in a clean computer with 
 * memsize bytes of "RAM".  If verbose is nonzero, prints out each instruction
 * before executing it.  Returns the result of the EXIT instruction.
 */
int run_program (int num_instructions, Instruction *instructions, int memsize, int verbose);


// +---------------+--------------------------------------------------
// | Miscellaneous |
// +---------------+

/**
 * Get the opcode from an opcode name.  Returns -1 for not found.
 */
int opcode (char *opname);

/**
 * Print a program.
 */
void print_program (FILE *stream, int num_instructions, Instruction *instructions, int number_lines);


#endif // __STAC__H_
