// parser.mly

%{
  open Absyn
%}

%token <bool>          LOGIC
%token <int>           INTEGER
%token <string>        STRING
%token <float>         REAL
%token <Symbol.symbol> ID
%token                 IF THEN ELSE
%token                 WHILE DO BREAK
%token                 LET IN END
%token                 VAR 
%token                 FUNCTION 
%token                 TYPE
%token                 LPAREN "(" RPAREN ")"
%token                 COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" POW "^"
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF


%right THEN ELSE DO IN
%nonassoc ASSIGN
%left "|"
%left "&"
%nonassoc "=" "<>" ">" ">=" LE LT
%left "+" "-"
%left "*" "/" "%"
%right "^"

%start <Absyn.lexp> program

%%

program:
 | x = exp EOF                                              {x}

exp:
 | x = INTEGER                                              {$loc, IntExp x}
 | x = REAL                                                 {$loc, RealExp x}
 | x = LOGIC                                                {$loc, BoolExp x}
 | x = STRING                                               {$loc, StringExp x}
 | v = var                                                  {$loc, VarExp v}
 | "-" e = exp                                              {$loc, OpExp (MinusOp, ($loc, IntExp 0), e)}
 | x = exp "+" y = exp                                      {$loc, OpExp (PlusOp, x, y)}
 | x = exp "-" y = exp                                      {$loc, OpExp (MinusOp, x, y)}
 | x = exp "*" y = exp                                      {$loc, OpExp (TimesOp, x, y)}
 | x = exp "/" y = exp                                      {$loc, OpExp (DivOp, x, y)}
 | x = exp "%" y = exp                                      {$loc, OpExp (ModOp, x, y)}
 | x = exp "^" y = exp                                      {$loc, OpExp (PowOp, x, y)}
 | x = exp "=" y = exp                                      {$loc, OpExp (EqOp, x, y)}
 | x = exp "<>" y = exp                                     {$loc, OpExp (NeOp, x, y)}
 | x = exp ">" y = exp                                      {$loc, OpExp (GtOp, x, y)}
 | x = exp ">=" y = exp                                     {$loc, OpExp (GeOp, x, y)}
 | x = exp "<" y = exp                                      {$loc, OpExp (LtOp, x, y)}
 | x = exp "<=" y = exp                                     {$loc, OpExp (LeOp, x, y)}
 | x = exp "&" y = exp                                      {$loc, OpExp (AndOp, x, y)}
 | x = exp "|" y = exp                                      {$loc, OpExp (OrOp, x, y)}

 | IF x = exp THEN y = exp ELSE z = exp                     {$loc, IfExp(x, y, Some(z))}
 | IF x = exp THEN y = exp                                  {$loc, IfExp(x, y, None)}
 | WHILE t = exp DO b = exp                                 {$loc, WhileExp (t, b)}
 | BREAK                                                    {$loc, BreakExp}
 | LET d = decs IN e = exp END                              {$loc, LetExp (d, e)}
 | func = ID "(" arguments = separated_list(",", exp) ")"   {$loc, CallExp(func, arguments)}
 | "(" exps = separated_list(";", exp) ")"                  {$loc, SeqExp exps}
 | v = var ":=" e = exp                                     {$loc, AssignExp (v, e)}
 

decs:
 | l = list(dec) {l}

dec:
 | v = vardec {v}
 | t = nonempty_list(typedec)                               {$loc, MutualTypeDecs t}
 | x = nonempty_list(funcdec)                               {$loc, MutualFunctionDecs x}

vardec:
 | VAR v=ID ":" t=ID ":=" e=exp                             {$loc, VarDec (v, Some ($loc(t), t), e)}
 | VAR v=ID ":=" e=exp                                      {$loc, VarDec (v, None, e)}

var:
 | x = ID                                                   {$loc, SimpleVar x}

typeCons:
 | x = ID                                                   {$loc, NameCons(x)}

argument:
 | x = ID ":" y = ID                                        {$loc, (x, y)}

typedec:
 | TYPE name = ID ":=" t = typeCons                         {$loc, (name, t)}

funcdec:
| FUNCTION x = ID "(" ps = separated_list(",", argument) ")" "=" e = exp {$loc, (x, ps, None, e)}
| FUNCTION x = ID "(" ps = separated_list(",", argument) ")" ":" v = ID "=" e = exp {$loc, (x, ps, Some ($loc(v), v), e)}