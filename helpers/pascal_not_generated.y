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

int 
  : _INTEGER
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_i_attribute (attributes, "value", atoi (yytext));
      Node *node = new_tnode (_INTEGER, attributes);
      $$ = node;
    }

real 
  : _REAL
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_d_attribute (attributes, "value", atof (yytext));
      Node *node = new_tnode (_REAL, attributes);
      $$ = node;
    }

str
  : _STRING
    {
      AttributeSet *attributes = new_attribute_set (1);
      set_s_attribute (attributes, "name", strdup (yytext));
      Node *node = new_tnode (_STRING, attributes);
      $$ = node;
    }
  ;

