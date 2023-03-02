%token PLUS
%token MINUS
%token <int> INT
%token <string> VAR
%token LT
%token LEQ
%token EQ
%token AND
%token OR
%token NOT
%token <bool> BOOL
%token ASIGN
%token SKIP
%token SEQ
%token IF
%token THEN
%token ELSE
%token IFTHEN
%token WHILE
%token DO
%token FINISH
%token OUTPUT
%token LEFTPAR
%token RIGHTPAR
%token SEMICOL

%token EOF

%start <Imp.cmd> prog

%%

prog:
  | e = cmd; EOF { e }
  ;

cmd:
  | LEFTPAR; c = cmd; RIGHTPAR { c }
  | OUTPUT; a = aexp { Imp.Output(a) }
  | name = VAR; ASIGN; value = aexp { Imp.Asgn(name, value) }
  | SKIP { Imp.Skip }
  | c1 = cmd; SEMICOL; c2 = cmd { Imp.Seq(c1, c2) }
  | IF; b = bexp; THEN; c1 = cmd; ELSE; c2 = cmd; IFTHEN { Imp.IfElse(b, c1, c2) }
  | WHILE; b = bexp; DO; c = cmd; FINISH { Imp.While(b, c) }
  ;

aexp:
  | LEFTPAR; e = aexp; RIGHTPAR { e }
  | i = INT { Imp.Int i }
  | v = VAR { Imp.Var v }
  | e1 = aexp; PLUS; e2 = aexp { Imp.Plus(e1, e2) }
  | e1 = aexp; MINUS; e2 = aexp { Imp.Minus(e1, e2) }
  ;

bexp:
  | LEFTPAR; e = bexp; RIGHTPAR { e }
  | b = Bool { Imp.Bool(b) }
  | e1 = aexp; LT; e2 = aexp { Imp.Lt(e1, e2) }
  | e1 = aexp; LEQ; e2 = aexp { Imp.Leq(e1, e2) }
  | e1 = aexp; EQ; e2 = aexp { Imp.Eq(e1, e2) }
  | e1 = bexp; AND; e2 = bexp { Imp.And(e1, e2) }
  | e1 = bexp; OR; e2 = bexp { Imp.Or(e1, e2) }
  | NOT; e = bexp { Imp.Not(e) }
  ;
