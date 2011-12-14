/**
 * staclib.c - A simple three-address code implementation.
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

#include <stdlib.h>	// For malloc and more
#include <stdio.h>	// For input and output
#include <string.h>     // For strcmp
#include "stac.h"


// +-----------+------------------------------------------------------
// | Constants |
// +-----------+

/**
 * The maximum number of registers.
 */
#define REGISTERS 128

/**
 * The initial number of labels.  We may allow this to expand.
 */
#define LABELS 128


// +-------+----------------------------------------------------------
// | Types |
// +-------+

/**
 * Information on a label.
 */
struct LabelInfo
  { 
    char *name;
    int instruction;
  };
typedef struct LabelInfo LabelInfo;

/**
 * Representation of memory.
 */
struct Memory
  {
    int size;   // The amount of memory we have
    unsigned char *data; // The actual data
  };
typedef struct Memory Memory;

/**
 * Values that can be stored.
 */
union Value
  {
    int i;
    float f;
    char c;
    char *s;
  };
typedef union Value Value;

/**
 * Information stored in one register.
 */
struct RegInfo
  {
    char *name;
    Value value;
    StacParameter *param;
  };
typedef struct RegInfo RegInfo;



// +--------------------+---------------------------------------------
// | Exported Variables |
// +--------------------+

StacParameter *SP = NULL;
StacParameter *BP = NULL;
StacParameter *HP = NULL;
StacParameter *R0 = NULL;
StacParameter *PC = NULL;


// +------------------+-----------------------------------------------
// | Global Variables |
// +------------------+

/**
 * Information on all of the registers.  (Their name, their value,
 * and even a "Parameter" type for them.)
 */
static RegInfo registers[REGISTERS];

/**
 * How many registers are active?
 */
static int active_registers = 0;

/**
 * How many registers are possible?
 */
static int max_registers = REGISTERS;

/**
 * Information on all of the labels.  For each label, we store name
 * and location.
 */
static LabelInfo labels[LABELS];

/**
 * How many labels are used.
 */
static int num_labels = 0;

/**
 * How many labels are possible?
 */
static int max_labels = LABELS;


// +---------------+--------------------------------------------------
// | Misc. Helpers |
// +---------------+

/**
 * Get a response from the user.  Reads one character, then clears
 * the rest of the line.
 */
char
read_response ()
{
  int ch = getchar ();
  char result;
  if (ch == '\n')
    return read_response ();
  if (ch == EOF)
    return '\0';
  result = (char) ch;
  while (((ch = getchar ()) != EOF) && (ch != '\n'))
    ;
  return result;
} // read_response

/**
 * Determine if two strings are equal, dealing appropriately with NULL.
 */
int
string_equal (char *str0, char *str1)
{
  if (str0 == str1)
    return 1;
  else if (str0 == NULL)
    return 0;
  else if (str1 == NULL)
    return 0;
  else
    return ! strcmp (str0, str1);
} // string_equal


// +-----------------------------+------------------------------------
// | Local Computation Utilities |
// +-----------------------------+

/**
 * Find the index of a register given its name.  Returns -1 if not found.
 */
int 
regnum (char *name)
{
  int i;
  for (i = 0; i < active_registers; i++)
    if (string_equal (name, registers[i].name))
      return i;
  return -1;
} // regnum


// +-------------------+----------------------------------------------
// | Retrieving Values |
// +-------------------+

/**
 * Get a character value stored at an absolute address.
 */
char
absolute_cget (int loc, Memory *mem)
{
  return *((char *) (mem->data + loc));
} // absolute_cget

/**
 * Get a float value stored at an absolute address.
 */
float
absolute_fget (int loc, Memory *mem)
{
  return *((float *) (mem->data + loc));
} // absolute_fget

/**
 * Get an integer value stored at an absolute address.
 */
int
absolute_iget (int loc, Memory *mem)
{
  return *((int *) (mem->data + loc));
} // absolute_iget

/**
 * Get the location associated with a label.  Returns -1 for "no such label".
 */
Value
label_get (char *label)
{
  int i;
  Value val;
  for (i = 0; i < num_labels; i++)
    {
      if (! strcmp (labels[i].name, label))
        {
          val.i = labels[i].instruction;
          return val;
        }
    } // for
  // Whoops.  It's not there.
  fprintf (stderr, "Unknown label: %s\n", label);
  val.i = -1;
  return val;
} // label_get

/**
 * Get the value stored in a register.
 */
Value
register_get (Register reg)
{
  return registers[reg.index].value;
} // register_get

/**
 * Get a float stored relative to a pointer address.
 */
float
relative_fget (RelAddr a, Memory *mem)
{
  int address = register_get (a.reg).i + a.offset;
  return absolute_fget (address, mem);
} // relative_fget

/**
 * Get an integer stored relative to a pointer address.
 */
int
relative_iget (RelAddr a, Memory *mem)
{
  int address = register_get (a.reg).i + a.offset;
  return absolute_iget (address, mem);
} // relative_iget

/**
 * Get the value associated with a parameter.
 */
Value
param_get (StacParameter *param, Memory *mem)
{
  Value val;
  switch (param->type)
    {
      case ICONSTANT:
        val.i = param->info.i;
        return val;

      case FCONSTANT:
        val.f = param->info.f;
        return val;

      case LABEL:
        return label_get (param->info.s);

      case REGISTER:
        return register_get (param->info.r);

      case SCONSTANT:
        val.s = param->info.s;
        return val;

      default:
        return (Value) 0;
    } // switch
} // param_get

/**
 * Get an integer value associated with a parameter.
 */
float
param_fget (StacParameter *param, Memory *mem)
{
  switch (param->type)
    {
      case ABSOLUTE:
        return absolute_fget (param->info.i, mem);
      case RELATIVE:
        return relative_fget (param->info.a, mem);
      default:
        return param_get (param, mem).f;
    }
} // param_fget

/**
 * Get an integer value associated with a parameter.
 */
int
param_iget (StacParameter *param, Memory *mem)
{
  switch (param->type)
    {
      case ABSOLUTE:
        return absolute_iget (param->info.i, mem);
      case RELATIVE:
        return relative_iget (param->info.a, mem);
      default:
        return param_get (param, mem).i;
    } // switch
} // iget

/**
 * Get a string value associated with a parameter.
 */
char *
param_sget (StacParameter *param, Memory *mem)
{
  switch (param->type)
    {
      default:
        return param_get (param, mem).s;
    }  // switch
} // param_sget 


// +----------------+-------------------------------------------------
// | Storing Values |
// +----------------+

/**
 * Store an integer value at an absolute address.
 */
int
absolute_fstore (int loc, Memory *mem, float val)
{
  if ((loc < 0) || (loc > (mem->size - 4)))
    {
      fprintf (stderr, "Address out of bounds: %d\n", loc);
      return 0;
    }
  *((float *) (mem->data + loc)) = val;
  return 1;
} // absolute_fstore

/**
 * Store an integer value at an absolute address.
 */
int
absolute_istore (int loc, Memory *mem, int val)
{
  if ((loc < 0) || (loc > (mem->size - 4)))
    {
      fprintf (stderr, "Address out of bounds: %d\n", loc);
      return 0;
    }
  *((int *) (mem->data + loc)) = val;
  return 1;
} // absolute_istore

/**
 * Store a value in a register.
 */
int
register_store (Register reg, Value val)
{
  registers[reg.index].value = val;
  return 1;
} // register_store

/**
 * Store a float value at a relative address.
 */
int
relative_fstore (RelAddr a, Memory *mem, float val)
{
  int address = register_get (a.reg).i + a.offset;
  return absolute_fstore (address, mem, val);
} // relative_fstore

/**
 * Store an integer value at a relative address.
 */
int
relative_istore (RelAddr a, Memory *mem, int val)
{
  int address = register_get (a.reg).i + a.offset;
  return absolute_istore (address, mem, val);
} // relative_istore

/**
 * Store a value in a parameter
 */
int
param_store (StacParameter *param, Memory *mem, Value val)
{
  switch (param->type)
    {
      case REGISTER:
        return register_store (param->info.r, val);
      default:
        return 0;
    } // switch
} // param_store

/**
 * Store a float value in a parameter.
 */
int
param_fstore (StacParameter *param, Memory *mem, float f)
{
  Value val;
  switch (param->type)
    {
      case ABSOLUTE:
        return absolute_fstore (param->info.i, mem, f);
      case RELATIVE:
        return relative_fstore (param->info.a, mem, f);
      default:
        val.f = f;
        return param_store (param, mem, val);
    }
} // param_fstore

/**
 * Store an integer value in a parameter.
 */
int
param_istore (StacParameter *param, Memory *mem, int i)
{
  Value val;
  switch (param->type)
    {
      case ABSOLUTE:
        return absolute_istore (param->info.i, mem, i);
      case RELATIVE:
        return relative_istore (param->info.a, mem, i);
      default:
        val.i = i;
        return param_store (param, mem, val);
    } // switch
} // param_istore


// +-------------------+----------------------------------------------
// | Program Execution |
// +-------------------+

void
store_label (char *name, int instruction)
{
  if (num_labels >= max_labels)
    {
      // We should probably expand the table, but ...
      fprintf (stderr, 
               "Exceeded number of labels (%d) with label '%s', instruction %d.\n",
               max_labels, name, instruction);
    } // if
  labels[num_labels].name = name;
  labels[num_labels].instruction = instruction;
  ++num_labels;
} // store_label

/**
 * Process the labels, assigning a location to each.
 */
void
process_labels (int num_instructions, Instruction *instructions)
{
  int i;
  for (i = 0; i < num_instructions; i++)
    {
      if (instructions[i].op == _LABL)
        {
          store_label (instructions[i].params[0]->info.s, i);
        } // if it's a label operation
    } // for
} // process_labels

/**
 * Jump to the location given in a parameter.
 */
int
jump (int location)
{
  Value newloc;
  // We subtract 1 because we increment the pc before the next instruction.
  newloc.i = location-1;
  register_store (PC->info.r, newloc);
  return 1;
} // jump

int
execute (Instruction *instruction, Memory *mem)
{
  int ileft, iright;    // Left and right arguments
  float fleft, fright;  // Left and right arguments
  // Commented out val because unused
  //Value val;            // A generic value, for when we need values
  StacParameter *target = instruction->params[0];
  
  // The following case statement is mostly in alphabetical order for
  // convenience.
  switch (instruction->op)
    {
      case F2I:
        fleft = param_fget (instruction->params[1], mem);
        return param_istore (target, mem, (int) fleft);

      case FADD:
      case FDIV:
      case FMUL:
      case FSUB:
      case JEQF:
      case JGTF:
      case JLEF:
      case JNEF:
        fleft = param_fget (instruction->params[1], mem);
        fright = param_fget (instruction->params[2], mem);
        switch (instruction->op)
          {
            case FADD:
              return param_fstore (target, mem, fleft+fright);
            case FDIV:
              return param_fstore (target, mem, fleft/fright);
            case FMUL:
              return param_fstore (target, mem, fleft*fright);
            case FSUB:
              return param_fstore (target, mem, fleft-fright);
            case JEQF:
              return (fleft == fright) ? jump (param_iget (target, mem)) : 1;
            case JGTF:
              return (fleft > fright) ? jump (param_iget (target, mem)) : 1;
            case JLEF:
              return (fleft <= fright) ? jump (param_iget (target, mem)) : 1;
            case JNEF:
              return (fleft != fright) ? jump (param_iget (target, mem)) : 1;
            default:
              return 1;  
          } // inner switch

      case FMOV:
        fleft = param_fget (instruction->params[1], mem);
        return param_fstore (target, mem, fleft);

      case FREAD:
        if (! scanf ("%f", &fleft))
          return 0;
        return param_fstore (target, mem, fleft);

      case FWRITE:
        printf ("%f", param_fget (target, mem));
        return 1;

      case I2F:
        ileft = param_iget (instruction->params[1], mem);
        return param_fstore (target, mem, (float) ileft);

      case IADD:
      case IDIV:
      case IMOD:
      case IMUL:
      case ISUB:
      case JEQI:
      case JGTI:
      case JLEI:
      case JNEI:
        ileft = param_iget (instruction->params[1], mem);
        iright = param_iget (instruction->params[2], mem);
        switch (instruction->op)
          {
            case IADD:
              return param_istore (target, mem, ileft+iright);
            case IDIV:
              return param_istore (target, mem, ileft/iright);
            case IMOD:
              return param_istore (target, mem, ileft%iright);
            case IMUL:
              return param_istore (target, mem, ileft*iright);
            case ISUB:
              return param_istore (target, mem, ileft-iright);
            case JEQI:
              return (ileft == iright) ? jump (param_iget (target, mem)) : 1;
            case JGTI:
              return (ileft > iright) ? jump (param_iget (target, mem)) : 1;
            case JLEI:
              return (ileft <= iright) ? jump (param_iget (target, mem)) : 1;
            case JNEI:
              return (ileft != iright) ? jump (param_iget (target, mem)) : 1;
            default:
              return 1;  
          } // inner switch

      case IMOV:
        ileft = param_iget (instruction->params[1], mem);
        return param_istore (target, mem, ileft);

      case IREAD:
        if (! scanf ("%d", &ileft))
          return 0;
        return param_istore (target, mem, ileft);

      case IWRITE:
        printf ("%d", param_iget (target, mem));
        return 1;

      case JMP:
        return jump (param_iget (target, mem));

      case _LABL:
        return 1;

      case NOOP:
        return 1;

      case SWRITE:
        printf ("%s", param_sget (target, mem));
        return 1;

      case WRITELN:
        printf ("\n");
        return 1;

      default:
        return 0;
    } // switch
} // execute

// +------------------------+-----------------------------------------
// | Local Output Utilities |
// +------------------------+

/**
 * Convert an opcode to a string.
 */
char *
opname (OpCode op)
{
#define opcase(OP) case OP: return #OP
  switch (op)
    {
      opcase(NOOP);
      opcase(JMP);
      opcase(JEQI);
      opcase(JNEI);
      opcase(JGTI);
      opcase(JLEI);
      opcase(JEQF);
      opcase(JNEF);
      opcase(JGTF);
      opcase(JLEF);
      opcase(IMOV);
      opcase(FMOV);
      opcase(SMOV);
      opcase(CMOV);
      opcase(I2F);
      opcase(F2I);
      opcase(C2I);
      opcase(IADD);
      opcase(ISUB);
      opcase(IMUL);
      opcase(IDIV);
      opcase(IMOD);
      opcase(FADD);
      opcase(FSUB);
      opcase(FMUL);
      opcase(FDIV);
      opcase(IREAD);
      opcase(FREAD);
      opcase(SREAD);
      opcase(CREAD);
      opcase(IWRITE);
      opcase(FWRITE);
      opcase(SWRITE);
      opcase(CWRITE);
      opcase(WRITELN);
      opcase(EXIT);
      opcase(_LABL);
      default:
        return ("UNKNOWN OP");
    } // switch
#undef opcase
} // opname

/**
 * Print out one parameter.  
 */
void
print_parameter (FILE *stream, StacParameter *param)
{
  switch (param->type)
    {
      case NONE:
        fprintf (stream, "NONE");
        break;

      case ICONSTANT:
        fprintf (stream, "%d", param->info.i);
        break;

      case FCONSTANT:
        fprintf (stream, "%f", param->info.f);
        break;

      case SCONSTANT:
        fprintf (stream, "\"%s\"", param->info.s);
        break;

      case CCONSTANT:
        fprintf (stream, "\'%c\'", param->info.c);
        break;

      case REGISTER:
        fprintf (stream, "%%%s", param->info.r.name);
        break;

      case ABSOLUTE:
        fprintf (stream, "%d", param->info.i);
        break;

      case RELATIVE:
        fprintf (stream, "%%%s(%d)", 
                 param->info.a.reg.name, param->info.a.offset);
        break;

      case LABEL:
        fprintf (stream, "%s", param->info.s);
        break;


      default:
        fprintf (stream, "???");
        break;
    } // switch
} // print_parameter

/**
 * Print out one instruction.
 */
void
print_instruction (FILE *stream, Instruction *instruction)
{
  int i;
  StacParameter *param;
  fprintf (stream, "%s ", opname (instruction->op));
  for (i = 0; i < 3; i++)
    {
      param = instruction->params[i];
      if ((param != NULL) && (param->type != NONE))
        {
          print_parameter (stream, param);
          fprintf (stream, " ");
        }
    } // for
  fprintf (stream, "\n");
} // print_instruction 



// +---------------------------+--------------------------------------
// | Primitive Building Blocks |
// +---------------------------+

void
stac_init (void)
{
  int i;
  if (active_registers == 0)
    {
      for (i = 0; i < REGISTERS; i++)
        registers[i].name = NULL;
    } // if (active_registers == 0)

  if (SP == NULL) 
    SP = get_register ("sp");
  if (BP == NULL)
    BP = get_register ("bp");
  if (HP == NULL)
    HP = get_register ("hp");
  if (R0 == NULL)
    R0 = get_register ("r0");
  if (PC == NULL)
    PC = get_register ("pc");
} // stac_init

void
build_instruction (Instruction *instruction, OpCode op, StacParameter *p1,
                   StacParameter *p2, StacParameter *p3)
{
  instruction->op = op;
  instruction->params[0] = p1;
  instruction->params[1] = p2;
  instruction->params[2] = p3;
} // build_instruction

StacParameter *
new_fconstant (float value)
{
  StacParameter *param = malloc (sizeof (StacParameter));
  if (param == NULL)
    return NULL;
  param->type = FCONSTANT;
  param->info.f = value;
  return param;
} // new_fconstant 

StacParameter *
new_iconstant (int value)
{
  StacParameter *param = malloc (sizeof (StacParameter));
  if (param == NULL)
    return NULL;
  param->type = ICONSTANT;
  param->info.i = value;
  return param;
} // new_iconstant 

StacParameter *
new_label (char *name)
{
  StacParameter *param = malloc (sizeof (StacParameter));
  if (param == NULL)
    return NULL;
  param->type = LABEL;
  param->info.s = name;
  return param;
} // new_label

StacParameter *
new_relative (Register reg, int offset)
{
  StacParameter *param = malloc (sizeof (StacParameter));
  if (param == NULL)
    return NULL;
  param->type = RELATIVE;
  param->info.a.reg = reg;
  param->info.a.offset = offset;
  return param;
} // new_relative

StacParameter *
new_sconstant (char *str)
{
  StacParameter *param = malloc (sizeof (StacParameter));
  if (param == NULL)
    return NULL;
  param->type = SCONSTANT;
  param->info.s = str;
  return param;
 } // new_sconstant

StacParameter *
get_register (char *name)
{
  // See if we already have a register with that name
  int num = regnum (name);
  if (num >= 0)
    return registers[num].param;

  // Well, we need to build a new register.  Make sure that's ok.
  if (active_registers >= max_registers)
    return NULL;

  // Allocate the new space.
  StacParameter *param = malloc (sizeof (StacParameter));
  if (param == NULL)
    return NULL;

  // Fill in the details
  param->type = REGISTER;
  param->info.r.name = name;
  param->info.r.index = active_registers;

  // Add it to the table
  registers[active_registers].name = name;
  registers[active_registers].value.i = 0;
  registers[active_registers].param = param;
  ++active_registers;

  // And we're done
  return param;
} // get_register


// +---------------+--------------------------------------------------
// | Miscellaneous |
// +---------------+

int
opcode (char *name)
{
  int i;
  for (i = 0; i < LAST_OP; i++)
    if (! strcmp (opname (i), name))
      return i;
  return -1;
} // opcode

int
run_program (int num_instructions, Instruction *instructions, int memsize, 
             int verbose)
{
  int pc = 0;
  char response;
  Memory mem;

  // Set up memory
  mem.size = memsize;
  mem.data = calloc (memsize, sizeof (unsigned char));
  if (mem.data == NULL)
    {
      fprintf (stderr, 
               "Could not allocate %d bytes of memory to run program.\n",
               memsize);
      return -1;
    } // if we can't allocate memory

  // Process the labels
  process_labels (num_instructions, instructions);

  // Set up registers
  param_istore (SP, &mem, memsize-4); 
  param_istore (BP, &mem, memsize-4);
  param_istore (HP, &mem, 0);

  // Execute instructions until we hit exit or other weird problem.
  while ( (pc >= 0)
          && (pc < num_instructions)
          && (instructions[pc].op != EXIT) )
    {
      if (verbose)
        {
          printf ("%3d ", pc);
          print_instruction (stdout, instructions+pc);
        }
      param_istore (PC, &mem, pc);
      if (! execute (instructions+pc, &mem))
        {
          fprintf (stderr, "Instruction failed: ");
          print_instruction (stderr, instructions+pc);
          fprintf (stderr, "Continue? (y/n) ");
          response = read_response ();
          if ((response != 'y') && (response != 'Y'))
            return -1;
        } // if the instruction did not execute
      // Move on to the next instruction
      pc = (register_get (PC->info.r)).i;
      pc++;
    } // while
  if ((pc < 0) || (pc >= num_instructions))
    {
      fprintf (stderr, "Program counter out of bounds: %d\n", pc);
      return -1;
    }
  if (verbose)
    {
      printf ("%3d ", pc);
      print_instruction (stdout, instructions+pc);
    }
  return param_iget (instructions[pc].params[0], &mem);
} // run_program

void 
print_program (FILE *stream, int num_instructions, Instruction *instructions, 
               int number_lines)
{
  int i;
  for (i = 0; i < num_instructions; i++)
    {
      if (number_lines)
        printf ("%4d ", i);
      print_instruction (stdout, instructions+i);
    }
} // print+_program
