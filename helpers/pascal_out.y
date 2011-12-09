program
  : program_head _SEMICOLON block _DOT 
    { $$ = simple_binary_tree (_program, $1, $3); }
  ;

program_head
  : _PROGRAM id 
    { $$ = $2; }
  | _PROGRAM id _LPAREN ids _RPAREN 
    { $$ = simple_binary_tree (_program_head, $2, $4); }
  ;

block
  : label_decl const_def_head type_def_head var_decl_head proc_and_func_decl compound_statetement 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_block, 6, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $2);
      set_child (node, 2, $3);
      set_child (node, 3, $4);
      set_child (node, 4, $5);
      set_child (node, 5, $6);
    }
  ;

ids
  : ids id 
    { $$ = simple_binary_tree (_ids, $1, $2); }
  | id 
    { $$ = $1; }
  ;

label_decl
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_label_decl, _empty_statement); }
  | _LABEL labels _SEMICOLON 
    { $$ = $2; }
  ;

const_def_head
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_const_def_head, _empty_statement); }
  | _CONST const_defs _SEMICOLON 
    { $$ = $2; }
  ;

type_def_head
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_type_def_head, _empty_statement); }
  | _TYPE type_defs _SEMICOLON 
    { $$ = $2; }
  ;

var_decl_head
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_var_decl_head, _empty_statement); }
  | _VAR var_decls _SEMICOLON 
    { $$ = $2; }
  ;

proc_and_func_decl
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_proc_and_func_decl, _empty_statement); }
  | proc_decls _SEMICOLON func_decls _SEMICOLON 
    { $$ = simple_binary_tree (_proc_and_func_decl, $1, $3); }
  ;

compound_statement
  : _BEGIN statements _END 
    { $$ = $2; }
  ;

labels
  : labels _COMMA label 
    { $$ = simple_binary_tree (_labels, $1, $3); }
  | label 
    { $$ = $1; }
  ;

label
  : int 
    { $$ = $1; }
  ;

const_defs
  : const_defs _SEMICOLON const_def 
    { $$ = simple_binary_tree (_const_defs, $1, $3); }
  | const_def 
    { $$ = $1; }
  ;

const_def
  : id _EQ const _SEMICOLON 
    { $$ = simple_binary_tree (_const_def, $1, $3); }
  ;

const
  : number 
    { $$ = $1; }
  | id 
    { $$ = $1; }
  | str 
    { $$ = $1; }
  ;

type_defs
  : type_defs _SEMICOLON type_def 
    { $$ = simple_binary_tree (_type_defs, $1, $3); }
  | type_def 
    { $$ = $1; }
  ;

type_def
  : id _EQ type_denoter _SEMICOLON 
    { $$ = simple_binary_tree (_type_def, $1, $3); }
  ;

var_decls
  : var_decls _SEMICOLON var_decl 
    { $$ = simple_binary_tree (_var_decls, $1, $3); }
  | var_decl 
    { $$ = $1; }
  ;

var_decl
  : ids _COLON type_denoter _SEMICOLON 
    { $$ = simple_binary_tree (_var_decl, $1, $3); }
  ;

proc_decls
  : proc_decls _SEMICOLON proc_decl 
    { $$ = simple_binary_tree (_proc_decls, $1, $3); }
  | proc_decl 
    { $$ = $1; }
  ;

proc_decl
  : proc_head _SEMICOLON id 
    { $$ = simple_binary_tree (_proc_decl, $1, $3); }
  | proc_id _SEMICOLON block 
    { $$ = simple_binary_tree (_proc_decl, $1, $3); }
  | proc_head _SEMICOLON block 
    { $$ = simple_binary_tree (_proc_decl, $1, $3); }
  ;

func_decls
  : func_decls _SEMICOLON func_decl 
    { $$ = simple_binary_tree (_func_decls, $1, $3); }
  | func_decl 
    { $$ = $1; }
  ;

func_decl
  : func_head _SEMICOLON id 
    { $$ = simple_binary_tree (_func_decl, $1, $3); }
  | func_id _SEMICOLON block 
    { $$ = simple_binary_tree (_func_decl, $1, $3); }
  | func_head _SEMICOLON block 
    { $$ = simple_binary_tree (_func_decl, $1, $3); }
  ;

statements
  : statements _SEMICOLON statement 
    { $$ = simple_binary_tree (_statements, $1, $3); }
  | statement 
    { $$ = $1; }
  ;

statement
  : label _COLON simple_or_struct_statement 
    { $$ = simple_binary_tree (_statement, $1, $3); }
  | simple_or_struct_statement 
    { $$ = $1; }
  ;

simple_or_struct_statement
  : simple_statement 
    { $$ = $1; }
  | struct_statement 
    { $$ = $1; }
  ;

simple_statement
  : empty_statement 
    { $$ = $1; }
  | assignment_statement 
    { $$ = $1; }
  | procedure_statement 
    { $$ = $1; }
  | goto_statement 
    { $$ = $1; }
  ;

struct_statement
  : compound_statement 
    { $$ = $1; }
  | conditional_statement 
    { $$ = $1; }
  | repetitive_statement 
    { $$ = $1; }
  | with_statement 
    { $$ = $1; }
  ;

proc_head
  : _PROCEDURE id formal_params 
    { $$ = simple_binary_tree (_proc_head, $2, $3); }
  ;

formal_params
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_formal_params, _empty_statement); }
  | _LBRACKET formal_param_sections _RBRACKET 
    { $$ = $2; }
  ;

formal_param_sections
  : formal_param_sections _SEMICOLON formal_param_section 
    { $$ = simple_binary_tree (_formal_param_sections, $1, $3); }
  | formal_param_section 
    { $$ = $1; }
  ;

formal_param_section
  : val_param_spec 
    { $$ = $1; }
  | _VAR ids _COLON id 
    { $$ = simple_binary_tree (_formal_param_section, $2, $4); }
  | proc_head 
    { $$ = $1; }
  | func_head 
    { $$ = $1; }
  | conform_array_spec 
    { $$ = $1; }
  ;

val_param_spec
  : ids _COLON id 
    { $$ = simple_binary_tree (_val_param_spec, $1, $3); }
  ;

conform_array_spec
  : val_conform_array_spec 
    { $$ = $1; }
  | var_conform_array_spec 
    { $$ = $1; }
  ;

val_conform_array_spec
  : ids _COLON conform_array_schema 
    { $$ = simple_binary_tree (_val_conform_array_spec, $1, $3); }
  ;

var_conform_array_spec
  : _VAR ids _COLON conform_array_schema 
    { $$ = simple_binary_tree (_var_conform_array_spec, $2, $4); }
  ;

conform_array_schema
  : packed_conform_array_schema 
    { $$ = $1; }
  | unpacked_conform_array_schema 
    { $$ = $1; }
  ;

packed_conform_array_schema
  : _PACKED _ARRAY _LBRACKET index_type_spec _RBRACKET _OF id 
    { $$ = simple_binary_tree (_packed_conform_array_schema, $4, $7); }
  ;

unpacked_conform_array_schema
  : _ARRAY _LBRACKET index_type_specs _RBRACKET _OF id_or_schema 
    { $$ = simple_binary_tree (_unpacked_conform_array_schema, $3, $6); }
  ;

index_type_specs
  : index_type_specs _SEMICOLON index_type_spec 
    { $$ = simple_binary_tree (_index_type_specs, $1, $3); }
  | index_type_spec 
    { $$ = $1; }
  ;

index_type_spec
  : id _ELLIPSES id _COLON id 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_index_type_spec, 3, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $3);
      set_child (node, 2, $5);
    }
  ;

id_or_schema
  : id 
    { $$ = $1; }
  | conform_array_schema 
    { $$ = $1; }
  ;

proc_id
  : _PROCEDURE id 
    { $$ = $2; }
  ;

func_id
  : _FUNCTION id 
    { $$ = $2; }
  ;

func_head
  : _FUNCTION id formal_params _COLON id 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_func_head, 3, attributes);
      set_child (node, 0, $2);
      set_child (node, 1, $3);
      set_child (node, 2, $5);
    }
  ;

type_denoter
  : id 
    { $$ = $1; }
  | new_type 
    { $$ = $1; }
  ;

new_type
  : new_ord_type 
    { $$ = $1; }
  | new_struct_type 
    { $$ = $1; }
  | new_pointer_type 
    { $$ = $1; }
  ;

new_ord_type
  : enum_type 
    { $$ = $1; }
  | subrange_type 
    { $$ = $1; }
  ;

new_struct_type
  : _PACKED unpacked_struct_type 
    { $$ = $2; }
  | unpacked_struct_type 
    { $$ = $1; }
  ;

new_pointer_type
  : _POINTER id 
    { $$ = $2; }
  ;

enum_type
  : _LPAREN ids _RPAREN 
    { $$ = $2; }
  ;

subrange_type
  : const _ELLIPSES const 
    { $$ = simple_binary_tree (_subrange_type, $1, $3); }
  ;

unpacked_struct_type
  : array_type 
    { $$ = $1; }
  | record_type 
    { $$ = $1; }
  | set_type 
    { $$ = $1; }
  | file_type 
    { $$ = $1; }
  ;

array_type
  : _ARRAY _LBRACKET ord_types _RBRACKET _OF type_denoter 
    { $$ = simple_binary_tree (_array_type, $3, $6); }
  ;

record_type
  : _RECORD fields _END 
    { $$ = $2; }
  ;

set_type
  : _SET _OF ord_type 
    { $$ = $3; }
  ;

file_type
  : _FILE _OF type_denoter 
    { $$ = $3; }
  ;

ord_types
  : ord_types _COMMA ord_type 
    { $$ = simple_binary_tree (_ord_types, $1, $3); }
  | ord_type 
    { $$ = $1; }
  ;

ord_type
  : new_ord_type 
    { $$ = $1; }
  | id 
    { $$ = $1; }
  ;

fields
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_fields, _empty_statement); }
  | record_sections optional_variant_part optional_semicolon 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_fields, 3, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $2);
      set_child (node, 2, $3);
    }
  | variant_part optional_semicolon 
    { $$ = simple_binary_tree (_fields, $1, $2); }
  ;

record_sections
  : record_sections _SEMICOLON record_section 
    { $$ = simple_binary_tree (_record_sections, $1, $3); }
  | record_section 
    { $$ = $1; }
  ;

optional_variant_part
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_optional_variant_part, _empty_statement); }
  | _SEMICOLON variant_part 
    { $$ = $2; }
  ;

optional_semicolon
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_optional_semicolon, _empty_statement); }
  | _SEMICOLON 
    { $$ = simple_unary_tree (_optional_semicolon, _SEMICOLON); }
  ;

variant_part
  : _CASE variant_selector _OF variants 
    { $$ = simple_binary_tree (_variant_part, $2, $4); }
  ;

variant_selector
  : id 
    { $$ = $1; }
  | id _COLON id 
    { $$ = simple_binary_tree (_variant_selector, $1, $3); }
  ;

variants
  : variants _SEMICOLON variant 
    { $$ = simple_binary_tree (_variants, $1, $3); }
  | variant 
    { $$ = $1; }
  ;

variant
  : consts _COLON _LPAREN fields _RPAREN 
    { $$ = simple_binary_tree (_variant, $1, $4); }
  ;

consts
  : consts _SEMICOLON const 
    { $$ = simple_binary_tree (_consts, $1, $3); }
  | const 
    { $$ = $1; }
  ;

record_section
  : ids _COLON type_denoter 
    { $$ = simple_binary_tree (_record_section, $1, $3); }
  ;

number
  : int 
    { $$ = $1; }
  | _REAL 
    { $$ = simple_unary_tree (_number, _REAL); }
  ;

assignment_statement
  : id _ASSIGN expr 
    { $$ = simple_binary_tree (_assignment_statement, $1, $3); }
  ;

procedure_statement
  : id parameters 
    { $$ = simple_binary_tree (_procedure_statement, $1, $2); }
  ;

parameters
  : actual_parameters_head 
    { $$ = $1; }
  | read_parameters_head 
    { $$ = $1; }
  | readln_parameters_head 
    { $$ = $1; }
  | write_parameters_head 
    { $$ = $1; }
  | writeln_parameters_head 
    { $$ = $1; }
  ;

actual_parameters_head
  : _LPAREN actual_parameters _RPAREN 
    { $$ = $2; }
  ;

actual_parameters
  : actual_parameters _COMMA actual_parameter 
    { $$ = simple_binary_tree (_actual_parameters, $1, $3); }
  | actual_parameter 
    { $$ = $1; }
  ;

actual_parameter
  : expr 
    { $$ = $1; }
  | var_access 
    { $$ = $1; }
  | _ID 
    { $$ = simple_unary_tree (_actual_parameter, _ID); }
  ;

read_parameters_head
  : _LPAREN optional_var_access var_accesses _RPAREN 
    { $$ = simple_binary_tree (_read_parameters_head, $2, $3); }
  ;

readln_parameters_head
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_readln_parameters_head, _empty_statement); }
  | _LPAREN _var_accesses _RPAREN 
    { $$ = simple_unary_tree (_readln_parameters_head, _LPAREN); }
  ;

write_parameters_head
  : LPAREN optional_var_access write_parameters _RPAREN 
    { $$ = simple_binary_tree (_write_parameters_head, $2, $3); }
  ;

writeln_parameters_head
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_writeln_parameters_head, _empty_statement); }
  | _LPAREN var_access optional_write_parameters _RPAREN 
    { $$ = simple_binary_tree (_writeln_parameters_head, $2, $3); }
  | _LPAREN write_parameter optional_write_parameters _RPAREN 
    { $$ = simple_binary_tree (_writeln_parameters_head, $2, $3); }
  ;

optional_var_access
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_optional_var_access, _empty_statement); }
  | var_access _COMMA 
    { $$ = $1; }
  ;

var_accesses
  : var_accesses _COMMA var_access 
    { $$ = simple_binary_tree (_var_accesses, $1, $3); }
  | var_access 
    { $$ = $1; }
  ;

write_parameters
  : write_parameters _COMMA write_parameter 
    { $$ = simple_binary_tree (_write_parameters, $1, $3); }
  | write_parameter 
    { $$ = $1; }
  ;

optional_write_parameters
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_optional_write_parameters, _empty_statement); }
  | _COMMA write_parameters 
    { $$ = $2; }
  ;

write_parameter
  : expr 
    { $$ = $1; }
  | expr _COLON expr 
    { $$ = simple_binary_tree (_write_parameter, $1, $3); }
  | expr _COLON expr _COLON expr 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_write_parameter, 3, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $3);
      set_child (node, 2, $5);
    }
  ;

goto_statement
  : _GOTO label 
    { $$ = $2; }
  ;

empty_statement
  : 
    { $$ = simple_unary_tree (_empty_statement, ;); }
  ;

conditional_statement
  : if_statement 
    { $$ = $1; }
  | case_statement 
    { $$ = $1; }
  ;

repetitive_statement
  : repeat_statement 
    { $$ = $1; }
  | while_statement 
    { $$ = $1; }
  | for_statement 
    { $$ = $1; }
  ;

with_statement
  : _WITH record_vars _DO statement 
    { $$ = simple_binary_tree (_with_statement, $2, $4); }
  ;

if_statement
  : _IF expr _THEN statement optional_else 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_if_statement, 3, attributes);
      set_child (node, 0, $2);
      set_child (node, 1, $4);
      set_child (node, 2, $5);
    }
  ;

case_statement
  : _CASE expr _OF consts_elems _SEMICOLON _END : _CASE expr _OF consts_elems _END 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_case_statement, 4, attributes);
      set_child (node, 0, $2);
      set_child (node, 1, $4);
      set_child (node, 2, $9);
      set_child (node, 3, $11);
    }
  ;

repeat_statement
  : _REPEAT statements _UNTIL expr 
    { $$ = simple_binary_tree (_repeat_statement, $2, $4); }
  ;

while_statement
  : _WHILE expr _DO statement 
    { $$ = simple_binary_tree (_while_statement, $2, $4); }
  ;

for_statement
  : _FOR id _ASSIGN expr to_or_downto expr _DO statement 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_for_statement, 5, attributes);
      set_child (node, 0, $2);
      set_child (node, 1, $4);
      set_child (node, 2, $5);
      set_child (node, 3, $6);
      set_child (node, 4, $8);
    }
  ;

optional_else
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_optional_else, _empty_statement); }
  | _ELSE statement 
    { $$ = $2; }
  ;

to_or_downto
  : _TO 
    { $$ = simple_unary_tree (_to_or_downto, _TO); }
  | _DOWNTO 
    { $$ = simple_unary_tree (_to_or_downto, _DOWNTO); }
  ;

record_vars
  : record_vars _COMMA record_var 
    { $$ = simple_binary_tree (_record_vars, $1, $3); }
  | record_var 
    { $$ = $1; }
  ;

record_var
  : var-access 
    { $$ = $1; }
  ;

var_access
  : id 
    { $$ = $1; }
  | component_var 
    { $$ = $1; }
  | var_access _POINTER 
    { $$ = $1; }
  ;

component_var
  : indexed_var 
    { $$ = $1; }
  | field_designator 
    { $$ = $1; }
  ;

indexed_var
  : var_access _LBRACKET exprs _RBRACKET 
    { $$ = simple_binary_tree (_indexed_var, $1, $3); }
  ;

field_designator
  : record_var _DOT id 
    { $$ = simple_binary_tree (_field_designator, $1, $3); }
  | id 
    { $$ = $1; }
  ;

exprs
  : exprs _COMMA expr 
    { $$ = simple_binary_tree (_exprs, $1, $3); }
  | expr 
    { $$ = $1; }
  ;

consts_elems
  : consts_elems _SEMICOLON consts_elem 
    { $$ = simple_binary_tree (_consts_elems, $1, $3); }
  | consts_elem 
    { $$ = $1; }
  ;

consts_elem
  : consts _COLON statement 
    { $$ = simple_binary_tree (_consts_elem, $1, $3); }
  ;

expr
  : expr addop term 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_expr, 3, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $2);
      set_child (node, 2, $3);
    }
  | term 
    { $$ = $1; }
  ;

term
  : term mulop factor 
    {
      AttributeSet *attributes = new_attribute_set (0);
      Node *node = new_nnode (_term, 3, attributes);
      set_child (node, 0, $1);
      set_child (node, 1, $2);
      set_child (node, 2, $3);
    }
  | factor 
    { $$ = $1; }
  ;

factor
  : int 
    { $$ = $1; }
  | id 
    { $$ = $1; }
  | _LPAREN expr _RPAREN 
    { $$ = $2; }
  ;

const
  : optional_sign number 
    { $$ = simple_binary_tree (_const, $1, $2); }
  | optional_sign id 
    { $$ = simple_binary_tree (_const, $1, $2); }
  | str 
    { $$ = $1; }
  ;

optional_sign
  : \\ epsilon 
    { $$ = simple_nonterm_unary_tree (_optional_sign, _empty_statement); }
  | _PLUS 
    { $$ = simple_unary_tree (_optional_sign, _PLUS); }
  | _DASH 
    { $$ = simple_unary_tree (_optional_sign, _DASH); }
  ;

%%
