%{

%}
%token FN 


%token LET


%token<string> UID LID STRING
%token<bool> BOOL
%token<int> INT
%token<char> CHAR
%token<float> FLOAT
%token UNDERSCORE

%token IMPL
%token SELF
%token CAST
%token BREAK
%token CASE
%token CONST
%token ELSE
%token ENUM
%token WHILE
%token FOR IN
%token IF
%token RETURN
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token PUB 
%token CONTINUE

%token ADD_EQ
%token SUB_EQ
%token MUL_EQ
%token DIV_EQ
%token MOD_EQ
%token OR_EQ
%token AND_EQ
%token XOR_EQ
%token LSHIFT_EQ
%token RSHIFT_EQ
%token LSHIFT "<<"
%token RSHIFT ">>"
%token EQEQ "=="
%token NE "!="
%token LE "<="
%token GE ">="
%token EQ "="
%token LT "<"
%token GT ">"
%token INC "++"
%token DEC "--"
%token ARROW "->"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token DIV "/"
%token MOD "%"
%token BANG "!"
%token ANDAND "&&"
%token OROR "||"
%token AND "&"
%token OR "|"
%token HAT "^"
%token QUESTION "?"
%token COLON ":"
%token NOT "~"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token LPAREN "("
%token RPAREN ")"
%token SEMICOLON ";"
%token COMMA ","
%token DOT "."
%token DEREF

%token EOF


%start<unit> translation_unit_file
%start<unit> expr_

%%

translation_unit_file:
| definition translation_unit_file
| definition EOF
    {}

definition:
| definition_
| PUB definition_
  {}

definition_:
| function_definition
| var_definition
| struct_definition
| enum_definition
| impl_definition
    {}

impl_definition:
| IMPL generics_params? UID generics_params? LBRACE list(fn_spec SEMICOLON {}) RBRACE
    {  }
    
function_definition:
| fn_spec compound_statement
    {  }
    
var_definition:
  LET decl EQ assignment_expr ";"
    {}


generics_params:
  LT separated_list(COMMA, ty) GT
    {}
decl:
  LID COLON ty
    {}

ret_ty:
  ARROW ty 
    {}

fn_spec:
  fn_path LPAREN separated_list(COMMA, decl) RPAREN ret_ty?
    {}


struct_definition:
| struct_path LBRACE list(decl SEMICOLON {}) RBRACE
 {}


enum_definition:
| enum_path LBRACE separated_list(COMMA, tag_spec) RBRACE
  {}


tag_spec:
| UID
| UID LPAREN separated_list(COMMA, ty) RPAREN
  {  }


fn_ty:
 FN generics_params?  LPAREN separated_list(COMMA, decl) RPAREN ret_ty
  {}

fn_path:
  FN LID generics_params
  {}

struct_path:
  STRUCT UID generics_params
  {}

enum_path:
  ENUM UID generics_params
  {}

built_in_ty:
| UID generics_params?
| BOOL
| CHAR
| INT
| FLOAT
| SELF
| LPAREN tuple RPAREN
  {}

tuple:
| ty COMMA ty
| tuple COMMA ty
  {}

base_ty:
| built_in_ty
| LPAREN ty RPAREN
  {}

ty_:
  base_ty 
| ty_ CONST
| ty_ STAR 
| ty_ LBRACKET constant_expr RBRACKET
 {}

ty:
| ty_ 
| fn_ty
  {}


literal:
| BOOL
| INT 
| FLOAT
| CHAR
| STRING
  {}

field_expr:
| "." LID EQ expr
| field_expr SEMICOLON "." LID EQ expr
  {}



primary_expr:
| LID
| FN LID generics_params
| LPAREN separated_list(COMMA,expr) RPAREN
| LBRACE field_expr RBRACE
| literal
    {}


postfix1_expr:
| primary_expr
| postfix1_expr "[" expr "]"
| postfix1_expr "(" argument_expr_list? ")"
| postfix1_expr "++"
| postfix1_expr "--"
| postfix1_expr DEREF
    {}

postfix2_expr:
| postfix1_expr
| UID
| UID LPAREN separated_list(COMMA,expr) RPAREN
| postfix2_expr "." LID
| postfix2_expr "." FN LID generics_params 
| postfix2_expr "." LID "(" argument_expr_list? ")"
| postfix2_expr "." FN LID generics_params "(" argument_expr_list? ")"
  {}

argument_expr_list:
| assignment_expr
| argument_expr_list "," assignment_expr
    {}

unary_expr:
| postfix2_expr
| unary_operator postfix2_expr
| SIZEOF LPAREN ty RPAREN
    {}

unary_operator:
| "&"
| "*"
| "+"
| "-"
| "~"
| "!"
    {}

cast_expr:
| unary_expr
| CAST LT ty GT LPAREN cast_expr RPAREN
  {}
multiplicative_operator:
  "*" | "/" | "%" {}

multiplicative_expr:
| cast_expr
| multiplicative_expr multiplicative_operator cast_expr
    {}

additive_operator:
  "+" | "-" {}

additive_expr:
| multiplicative_expr
| additive_expr additive_operator multiplicative_expr
    {}

shift_operator:
  "<<" | ">>" {}

shift_expr:
| additive_expr
| shift_expr shift_operator additive_expr
    {}

relational_operator:
  "<" | ">" | "<=" | ">=" {}

relational_expr:
| shift_expr
| relational_expr relational_operator shift_expr
    {}

equality_operator:
  "==" | "!=" {}

equality_expr:
| relational_expr
| equality_expr equality_operator relational_expr
    {}

and_expr:
| equality_expr
| and_expr "&" equality_expr
    {}

exclusive_or_expr:
| and_expr
| exclusive_or_expr "^" and_expr
    {}

inclusive_or_expr:
| exclusive_or_expr
| inclusive_or_expr "|" exclusive_or_expr
    {}

logical_and_expr:
| inclusive_or_expr
| logical_and_expr "&&" inclusive_or_expr
    {}

logical_or_expr:
| logical_and_expr
| logical_or_expr "||" logical_and_expr
    {}

conditional_expr:
| logical_or_expr
| logical_or_expr "?" expr ":" conditional_expr
    {}

assignment_expr:
| conditional_expr
| unary_expr assignment_operator assignment_expr
    {}

assignment_operator:
| "="
| MUL_EQ
| DIV_EQ
| MOD_EQ
| ADD_EQ
| SUB_EQ
| LSHIFT_EQ
| RSHIFT_EQ
| AND_EQ
| XOR_EQ
| OR_EQ
    {}

expr:
| assignment_expr
  {}

expr_:
| assignment_expr EOF
  {}

constant_expr:
| conditional_expr
    {}

statement:
| compound_statement
| expr_statement
| selection_statement
| iteration_statement
| jump_statement
    {}


compound_statement:
| "{" block_item_list? "}"
    {}

block_item_list:
| block_item_list? block_item
    {}

block_item:
| var_definition
| statement
    {}

expr_statement:
| expr? ";"
    {}

selection_statement:
| IF expr compound_statement ELSE compound_statement
| IF expr compound_statement
| SWITCH expr LBRACE list(CASE patterrn compound_statement{} ) RBRACE
    {}

iteration_statement:
| WHILE expr compound_statement
| FOR expr IN expr compound_statement
    {}

jump_statement:
| CONTINUE ";"
| BREAK ";"
| RETURN expr? ";"
    {}

patterrn:
| literal
| UNDERSCORE
| UID
| UID LPAREN separated_list(COMMA, patterrn) RPAREN
| LPAREN separated_list(COMMA, patterrn) RPAREN
  {}