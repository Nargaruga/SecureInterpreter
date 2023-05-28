// Token declarations
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token <string> STR

%token PLUS
%token MINUS
%token TIMES
%token DIV
%token EQ
%token GT
%token GTE
%token LT
%token LTE

%token LPAREN
%token RPAREN

%token AND
%token OR
%token NOT

%token END

%token IF
%token THEN
%token ELSE

%token LET
%token ASSIGN
%token IN

%token ENCLAVE
%token SECRET
%token GATEWAY

%token INCLUDE
%token EXECUTE

%token PRINT

%token EOF

// Operator associativity and precedence
%left PLUS MINUS
%left TIMES DIV

%left EQ
%left GT LT GTE LTE

%right NOT
%left AND
%left OR

// Type declarations
%type <Ast.expr> expr
%type <Ast.expr option> prog

// Entry point
%start prog

%%

// Production rules
prog:
    | EOF { None }
    | e = expr EOF { Some(e); }
    ;

expr:
    | i = INT { Ast.Int i }
    | MINUS; i = INT { Ast.Int (-i) }
    | b = BOOL { Ast.Bool b }
    | s = STR { Ast.String s }

    | id = ID { Ast.Var id }

    | LPAREN; arg = ID; RPAREN; body = expr { Ast.Fun(arg, body) }
    | f = ID; LPAREN; arg = expr; RPAREN; { Ast.Call(f, arg) }

    | e1 = expr; PLUS; e2 = expr { Ast.Binop (Ast.Add, e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Ast.Binop (Ast.Sub, e1, e2) }
    | e1 = expr; TIMES; e2 = expr { Ast.Binop (Ast.Mult, e1, e2) }
    | e1 = expr; DIV; e2 = expr { Ast.Binop (Ast.Div, e1, e2) }

    | LPAREN; e = expr; RPAREN { e }

    | e1 = expr; AND; e2 = expr { Ast.Binop (Ast.And, e1, e2) }
    | e1 = expr; OR; e2 = expr { Ast.Binop (Ast.Or, e1, e2) }
    | NOT; e = expr { Ast.Not e}

    | e1 = expr; EQ; e2 = expr { Ast.Binop (Ast.Eq, e1, e2) }
    | e1 = expr; GT; e2 = expr { Ast.Binop (Ast.Gt, e1, e2) }
    | e1 = expr; GTE; e2 = expr { Ast.Binop (Ast.Gte, e1, e2) }
    | e1 = expr; LT; e2 = expr { Ast.Binop (Ast.Lt, e1, e2) }
    | e1 = expr; LTE; e2 = expr { Ast.Binop (Ast.Lte, e1, e2) }

    | IF; guard = expr; THEN; then_body = expr; ELSE; else_body = expr; { Ast.IfThenElse(guard, then_body, else_body) }

    | LET; id = ID; ASSIGN; body = expr; IN; rest = expr; { Ast.Let(id, body, rest) }
    | LET; id = ID; arg = ID; ASSIGN; body = expr; IN; rest = expr; { Ast.Let(id, Ast.Fun(arg, body), rest) }

    | ENCLAVE; body = expr; END; rest = expr { Ast.Enclave(body, rest) }
    | SECRET; id = ID; ASSIGN; body = expr; IN; rest = expr; { Ast.Secret(id, body, rest) }
    | GATEWAY; id = ID; IN; rest = expr { Ast.Gateway(id, rest) }

    | INCLUDE; id = ID; ASSIGN; body = expr; END; rest = expr; { Ast.Include(id, body, rest) }
    | EXECUTE; LPAREN; id = ID; RPAREN; rest = expr; { Ast.Execute(id, rest) }

    | PRINT; LPAREN; e = expr; RPAREN; rest = expr { Ast.Print(e, rest) }
    ;

