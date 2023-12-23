%{
  open Ast
    let opt_to_list = function 
    | Some l -> l
    | None -> []

    let conv_ret = function
    | Some ty -> ty
    | None -> Tint
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
(*
%token SELF
%token CAST
*)
%token BREAK TBOOL TINT TCHAR TFLOAT
%token CASE
%token CONST
%token ELSE
%token ENUM
%token WHILE
%token FOR IN
%token IF
%token RETURN
%token SIZEOF

%token STRUCT
%token SWITCH
%token CONTINUE
(*
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
*)
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


%start<def list> top
%start<unit> expr_


%%

top:
| def top  { $1::$2 }
| def EOF
    { [$1] }

def:
| function_def
| var_def 
| struct_def
| enum_def
| impl_def
    { $1 }

impl_def:
| IMPL generics_params? UID generics_params? LBRACE list(method_def) RBRACE
    { Defimpl(opt_to_list $2, Tconstr($3,opt_to_list $4),$6) }

method_def:
    FN generics_params? option(LPAREN ty RPAREN {$2 }) LID generics_params? LPAREN separated_list(COMMA, decl) RPAREN ret_ty? compound_statement
    { 
      Defmethod($4,opt_to_list $2,$3,opt_to_list $5,conv_ret $9,$7,$10)
    }


function_def:
| fn_spec compound_statement
    { let (name,ty,tyl,params) = $1 in
      Deffun(name,ty,tyl,params,$2)
    }
    
var_def:
  LET decl EQ expr ";"
    { Deflet($2,$4) }


generics_params:
  LT separated_list(COMMA, ty) GT
    { $2 }
decl:
  LID COLON ty
    { ($1,$3) }

ret_ty:
  ARROW ty
    { $2 }

fn_spec:
  fn_path LPAREN separated_list(COMMA, decl) RPAREN ret_ty?
    { let (name, tyl) = $1 in
      (name,tyl,conv_ret $5,$3)
    }

struct_def:
| struct_path LBRACE list(decl SEMICOLON { $1 }) RBRACE
   { let (name,tyl) = $1 in
     Defstruct(name,tyl,$3)
   }


enum_def:
| enum_path LBRACE list(tag_spec SEMICOLON { $1 }) RBRACE
   { let (name,tyl) = $1 in
     Defenum(name,tyl,$3)
   }


tag_spec:
| UID  { ($1,Ttag) }
| UID LPAREN ty RPAREN { ($1,$3) }
| UID LPAREN ty COMMA separated_nonempty_list(COMMA, ty) RPAREN
  { ($1, Ttuple ($3::$5)) }


fn_ty:
 FN generics_params?  LPAREN separated_list(COMMA, decl) RPAREN ret_ty
  { Tfun(opt_to_list $2,$6,$4) }

fn_path:
  FN LID generics_params?
  { ($2,opt_to_list $3) }

struct_path:
  STRUCT UID generics_params?
    { ($2,opt_to_list $3) }

enum_path:
  ENUM UID generics_params?
    { ($2,opt_to_list $3) }

base_ty:
| UID generics_params?  { Tconstr($1,opt_to_list $2) }
| TBOOL  { Tbool }
| TCHAR  { Tchar}
| TINT   { Tint }
| TFLOAT { Tfloat }
| LPAREN ty COMMA separated_nonempty_list(COMMA,ty) RPAREN
  { Ttuple($2::$4) }
| LPAREN ty RPAREN
  { $2 }

ty_:
  base_ty 
  { $1 }
| ty_ CONST
  { $1 }
| ty_ STAR 
  { Tptr $1 }
| ty_ LBRACKET RBRACKET
  { Tlist $1 }

ty:
| ty_ 
| fn_ty
  { $1 }


literal:
| BOOL { Cbool $1 }
| INT { Cint $1 }
| FLOAT { Cfloat $1 }
| CHAR { Cchar $1 }
| STRING { Cstring $1 }

field_expr:
| "." LID EQ expr SEMICOLON
  { ($2,$4) }



primary_expr:
| LID  { Evar([],$1) }
| FN LID generics_params { Evar($3,$2) }
| LPAREN expr COMMA separated_nonempty_list(COMMA,expr) RPAREN { Etuple($2::$4) }
| LPAREN expr RPAREN  { $2 }
| LPAREN RPAREN  { Eunit }
| LBRACE nonempty_list(field_expr) RBRACE  { Estruct $2 }
| literal { Econstant $1 }


postfix1_expr:
| primary_expr  { $1 }
| postfix1_expr "[" expr "]" { EPostfix ($1,PIdx $3) }
| postfix1_expr "(" separated_list(COMMA,expr) ")" { EPostfix($1,PCall $3) }


postfix2_expr:
| postfix1_expr { $1 }
| UID { Etag $1 }
| UID LPAREN expr RPAREN { Econstruct($1,$3) }
| UID LPAREN expr COMMA separated_nonempty_list(COMMA,expr) RPAREN { Econstruct($1,Etuple($3::$5)) }
| postfix2_expr "." LID { EPostfix($1,PDot([],$3)) }
| postfix2_expr "." FN LID generics_params { EPostfix($1,PDot($5,$4)) } 
| postfix2_expr "." LID "(" separated_list(COMMA,expr) ")" 
  { EPostfix(EPostfix($1,PDot([],$3)),PCall( $5)) }
| postfix2_expr "." FN LID generics_params "(" separated_list(COMMA,expr) ")"
  { EPostfix(EPostfix($1,PDot($5,$4)),PCall( $7)) }
| postfix2_expr "++" { EPostfix($1,PInc) }
| postfix2_expr "--" { EPostfix($1,PDec) }
| postfix2_expr DEREF { EPostfix($1,PDeref) }


unary_expr:
| postfix2_expr  { $1 }
| unary_operator postfix2_expr  { EUnary($1,$2) }
| SIZEOF LPAREN ty RPAREN  { ESizeof($3) }

unary_operator:
| "&" { Ref }
| "+" { Plus }
| "-" {Minus }
| "~" { BitNot }
| "!" { LogNot }

cast_expr:
| unary_expr { $1 }
(* | CAST LT ty GT LPAREN cast_expr RPAREN  {} *)
multiplicative_operator:
  "*" { Mul} | "/" { Div } | "%" { Mod }

multiplicative_expr:
| cast_expr { $1 }
| multiplicative_expr multiplicative_operator cast_expr
    { EBinary($2,$1,$3) }

additive_operator:
  "+" { Add } | "-" { Sub }

additive_expr:
| multiplicative_expr { $1 }
| additive_expr additive_operator multiplicative_expr
    { EBinary($2,$1,$3) }

shift_operator:
  "<<" { LShift } | ">>" { RShift }

shift_expr:
| additive_expr { $1 }
| shift_expr shift_operator additive_expr
    { EBinary($2,$1,$3) }

relational_operator:
  "<" { Lt } | ">" { Gt } | "<=" { Le } | ">=" { Ge }

relational_expr:
| shift_expr { $1 }
| relational_expr relational_operator shift_expr
    { EBinary($2,$1,$3) }

equality_operator:
  "==" { Eq } | "!=" { Ne }

equality_expr:
| relational_expr { $1 }
| equality_expr equality_operator relational_expr
    { EBinary($2,$1,$3) }

and_expr:
| equality_expr { $1 }
| and_expr "&" equality_expr
    { EBinary(BitAnd,$1,$3) }

exclusive_or_expr:
| and_expr { $1 }
| exclusive_or_expr "^" and_expr
    { EBinary(BitXor,$1,$3) }

inclusive_or_expr:
| exclusive_or_expr { $1 }
| inclusive_or_expr "|" exclusive_or_expr
    { EBinary(BitOr,$1,$3) }

logical_and_expr:
| inclusive_or_expr { $1 }
| logical_and_expr "&&" inclusive_or_expr
    { EBinary(LogAnd,$1,$3) }

logical_or_expr:
| logical_and_expr { $1 }
| logical_or_expr "||" logical_and_expr
    { EBinary(LogOr,$1,$3) }

expr:
| logical_or_expr
  { $1 }

expr_:
| expr EOF
  {}

statement:
| var_statement
| compound_statement
| expr_statement
| selection_statement
| iteration_statement
| jump_statement
    { $1 }


var_statement:
  LET decl EQ expr ";"
    { SLet($2,$4) }

compound_statement:
| "{" list(statement) "}"
    { SStmts $2 }


expr_statement:
| expr? ";"
    { match $1 with
      | Some e -> SExpr e 
      | None -> SNone
    }

selection_statement:
| IF expr compound_statement ELSE compound_statement
  { SIfElse($2,$3,$5) }
| IF expr compound_statement
  { SIfElse($2,$3,SStmts []) }
| SWITCH expr LBRACE list(match_arm) RBRACE
    { SSwitch($2,$4) }

match_arm:
CASE pattern COLON compound_statement
  {
    ($2,$4)
  } 

iteration_statement:
| WHILE expr compound_statement
  { SWhile($2,$3) }
| FOR expr IN expr compound_statement
    { SFor($2,$4,$5) }

jump_statement:
| CONTINUE ";"  { SContinue }
| BREAK ";" { SBreak }
| RETURN  expr? ";" { SReturn $2 }

pattern:
| literal  { Pconstant $1 }
| UNDERSCORE { Pwild }
| LID { Pvar $1 }
| UID { Ptag $1 }
| LPAREN RPAREN { Punit }
| LPAREN pattern COMMA separated_nonempty_list(COMMA, pattern) RPAREN
  { Ptuple($2::$4) }
| UID LPAREN pattern RPAREN
  { Pconstruct($1,$3) }
| UID LPAREN pattern COMMA separated_nonempty_list(COMMA, pattern) RPAREN
  { Pconstruct($1,Ptuple($3::$5)) }