program
  : program_head _SEMICOLON block _DOT
  ;

program_head
  : _PROGRAM id
      | _PROGRAM id _LPAREN ids _RPAREN
    ;

block
  : label_decl const_def_head type_def_head var_decl_head proc_and_func_decl
    compound_statement
      ;

ids
  : ids id
  | id
      ;

label_decl
  : // epsilon
      | _LABEL labels _SEMICOLON
      ;

const_def_head
  : // epsilon
      | _CONST const_defs _SEMICOLON
      ;

type_def_head
  : // epsilon
      | _TYPE type_defs _SEMICOLON
      ;

var_decl_head
  : // epsilon
      | _VAR var_decls _SEMICOLON     
      ;
  
proc_and_func_decl
  : // epsilon
      | proc_decls _SEMICOLON func_decls _SEMICOLON
      ;

compound_statement
  : _BEGIN statements _END
      ;

labels
  : labels _COMMA label
      | label
      ;

label
  : int
      ;

const_defs
  : const_defs _SEMICOLON const_def
      | const_def
      ;

const_def
  : id _EQ const _SEMICOLON
      ; 

type_defs
  : type_defs _SEMICOLON type_def
      | type_def
      ;

type_def
  : id _EQ type_denoter _SEMICOLON
      ;

var_decls
  : var_decls _SEMICOLON var_decl
      | var_decl
      ;

var_decl
  : ids _COLON type_denoter _SEMICOLON
      ;

proc_decls
  : proc_decls _SEMICOLON proc_decl
      | proc_decl
      ;

proc_decl
  : proc_head _SEMICOLON id
      | proc_id _SEMICOLON block
      | proc_head _SEMICOLON block
      ;

func_decls
  : func_decls _SEMICOLON func_decl
      | func_decl
      ;

func_decl
  : func_head _SEMICOLON id
      | func_id _SEMICOLON block
      | func_head _SEMICOLON block
      ;

statements
  : statements _SEMICOLON statement 
    | statement
       ;

statement
  : label _COLON simple_or_struct_statement
      | simple_or_struct_statement
      ;

simple_or_struct_statement
  : simple_statement
      | struct_statement
      ;

simple_statement
  : empty_statement
      | assignment_statement
      | procedure_statement
      | goto_statement
      ;

struct_statement
  : compound_statement
      | conditional_statement
      | repetitive_statement
      | with_statement
      ;

proc_head
  : _PROCEDURE id formal_params
      ;

formal_params
  : // epsilon
  | _LBRACKET formal_param_sections _RBRACKET
      ;

formal_param_sections
  : formal_param_sections _SEMICOLON formal_param_section
      | formal_param_section
      ;

formal_param_section
  : val_param_spec
      | _VAR ids _COLON id
      | proc_head
      | func_head
      | conform_array_spec
      ;

val_param_spec
  : ids _COLON id
      ;

conform_array_spec
  : val_conform_array_spec
      | var_conform_array_spec
      ;

val_conform_array_spec
  : ids _COLON conform_array_schema
      ;

var_conform_array_spec
  : _VAR ids _COLON conform_array_schema
      ;

conform_array_schema
  : packed_conform_array_schema
      | unpacked_conform_array_schema
      ;

packed_conform_array_schema
  : _PACKED _ARRAY _LBRACKET index_type_spec _RBRACKET _OF id
      ;

unpacked_conform_array_schema
  : _ARRAY _LBRACKET index_type_specs _RBRACKET _OF id_or_schema
      ;

index_type_specs
  : index_type_specs _SEMICOLON index_type_spec
      | index_type_spec
      ;

index_type_spec
  : id _ELLIPSES id _COLON id
      ;

id_or_schema
  : id
      | conform_array_schema
      ;

proc_id
  : _PROCEDURE id
      ;

func_id
  : _FUNCTION id
      ;

func_head
  : _FUNCTION id formal_params _COLON id
      ;

type_denoter
  : id
      | new_type
      ;

new_type
  : new_ord_type
      | new_struct_type
      | new_pointer_type
      ;

new_ord_type
  : enum_type
      | subrange_type
      ;

new_struct_type
  : _PACKED unpacked_struct_type
      | unpacked_struct_type
      ;

new_pointer_type
  : _POINTER id
      ;

enum_type
  : _LPAREN ids _RPAREN
      ;

subrange_type
  : const _ELLIPSES const
      ;

unpacked_struct_type
  : array_type
      | record_type
      | set_type
      | file_type
  ;

array_type
  : _ARRAY _LBRACKET ord_types _RBRACKET _OF type_denoter
      ;

record_type
  : _RECORD fields _END
      ;

set_type
  : _SET _OF ord_type
      ;

file_type
  : _FILE _OF type_denoter
      ;

ord_types
  : ord_types _COMMA ord_type
      | ord_type
      ;

ord_type
  : new_ord_type
      | id
      ;

fields
  : // epsilon
  | record_sections optional_variant_part optional_semicolon
      | variant_part optional_semicolon
      ;
  
record_sections
  : record_sections _SEMICOLON record_section
      | record_section
      ;

optional_variant_part
  : // epsilon
  | _SEMICOLON variant_part
      ;

optional_semicolon
  : // epsilon
  | _SEMICOLON
      ;

variant_part
  : _CASE variant_selector _OF variants
      ;

variant_selector
  : id
      | id _COLON id
      ;

variants
  : variants _SEMICOLON variant
      | variant
      ;

variant
  : consts _COLON _LPAREN fields _RPAREN
      ;

consts
  : consts _SEMICOLON const
      | const
      ;

record_section
  : ids _COLON type_denoter
      ;

number
  : int
      | real
      ;

assignment_statement
  : id _ASSIGN expr
      ;

procedure_statement
  : id parameters
      ;

parameters
  : actual_parameters_head
      | read_parameters_head
      | readln_parameters_head
      | write_parameters_head
      | writeln_parameters_head
      ;

actual_parameters_head
  : _LPAREN actual_parameters _RPAREN
      ;

actual_parameters
  : actual_parameters _COMMA actual_parameter
      | actual_parameter
      ;

actual_parameter
  : expr
      | var_access
      | id
      ;

read_parameters_head
  : _LPAREN optional_var_access var_accesses _RPAREN
      ;

readln_parameters_head
  : // epsilon
  | _LPAREN var_accesses _RPAREN
      ;
 
write_parameters_head
  : _LPAREN optional_var_access write_parameters _RPAREN
      ;

writeln_parameters_head
  : // epsilon
  | _LPAREN var_access optional_write_parameters _RPAREN
      | _LPAREN write_parameter optional_write_parameters _RPAREN
      ;

optional_var_access
  : // epsilon
  | var_access _COMMA
      ;

var_accesses
  : var_accesses _COMMA var_access
      | var_access
      ;

write_parameters
  : write_parameters _COMMA write_parameter
      | write_parameter
      ;

optional_write_parameters
  : // epsilon
  | _COMMA write_parameters
      ;

write_parameter
  : expr 
      | expr _COLON expr
      | expr _COLON expr _COLON expr
      ;

goto_statement
  : _GOTO label
      ;

empty_statement
  :
     ;

conditional_statement
  : if_statement
      | case_statement
      ;

repetitive_statement
  : repeat_statement
      | while_statement
      | for_statement
      ;

with_statement
  : _WITH record_vars _DO statement
      ;

if_statement
  : _IF expr _THEN statement optional_else
      ;

case_statement
  : _CASE expr _OF consts_elems _SEMICOLON _END
      | _CASE expr _OF consts_elems _END
      ;

repeat_statement
  : _REPEAT statements _UNTIL expr
      ;

while_statement
  : _WHILE expr _DO statement
      ;

for_statement
  : _FOR id _ASSIGN expr to_or_downto expr _DO statement
      ;

optional_else
  : // epsilon
      | _ELSE statement
      ;

to_or_downto
  : _TO
      | _DOWNTO
      ;

record_vars
  : record_vars _COMMA record_var
      | record_var
      ;

record_var
  : var_access
      ;

var_access
  : id
      | component_var
      | var_access _POINTER
      ;

component_var
  : indexed_var
      | field_designator
      ;

indexed_var
  : var_access _LBRACKET exprs _RBRACKET
      ;

field_designator
  : record_var _DOT id
      | id
      ;

exprs
  : exprs _COMMA expr
      | expr
      ;

consts_elems
  : consts_elems _SEMICOLON consts_elem
      | consts_elem
      ;

consts_elem
  : consts _COLON statement
      ;

expr
  : expr addop term
  | term
 ;

term
  : term mulop factor
  | factor
 ;

factor
  : int
      | id
      | _LPAREN expr _RPAREN
      ;


const
  : optional_sign number
      | optional_sign id
      | str
      ;

optional_sign
  : // epsilon
      | _PLUS
      | _DASH
      ;

%%
