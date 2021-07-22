%%

%name PlcParser

%pos int

%nonterm Prog of expr | Decl of expr | Expr of expr | 
    Atomexpr of expr | Appexpr of expr | Const of expr |
    Comps of expr list | Matchexpr of (expr option * expr) list | 
    Condexpr of expr option | Args of (plcType*string) list | 
    Params of (plcType*string) list | Typedvar of (plcType*string) |
    Type of plcType | Atomtype of plcType | Types of plcType list


%term VAR | FUN | REC | NAME of string | COLON | SEMICOLON | IF |
    THEN | ELSE | MATCH | WITH | NOT | HD | TL |
    ISE | PRINT | AND | LPAR | RPAR | RBRACK | LBRACK | 
    RBRACE | LBRACE | PLUS | MINUS | TIMES | DIV | EQUAL | 
    DIFF | LESS | LESSEQ | DOUBLECOLON | NAT of int | 
    FN | ARROW | END | TRUE | FALSE | COMMA |
    PIPE | UNDER | NIL | BOOL | INT | EOF | EQARROW

%right SEMICOLON ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQUAL DIFF
%left LESS LESSEQ
%right DOUBLECOLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HD TL ISE PRINT NAME
%left LBRACK

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr) | 
       Decl (Decl)

Decl : VAR NAME EQUAL Expr SEMICOLON Prog (Let(NAME, Expr, Prog)) |
       FUN NAME Args EQUAL Expr SEMICOLON Prog (Let(NAME, makeAnon(Args, Expr), Prog)) |
       FUN REC NAME Args COLON Type EQUAL Expr SEMICOLON Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr : Atomexpr (Atomexpr) |
       Appexpr (Appexpr) |
       IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3)) |
       MATCH Expr WITH Matchexpr(Match(Expr, Matchexpr)) |
       NOT Expr (Prim1("!", Expr)) |
       MINUS Expr (Prim1("-", Expr)) |
       HD Expr (Prim1("hd", Expr)) |
       TL Expr (Prim1("tl", Expr)) |
       ISE Expr (Prim1("ise", Expr)) | 
       PRINT Expr (Prim1("print", Expr)) |
       Expr AND Expr (Prim2("&&", Expr1, Expr2)) |
       Expr PLUS Expr (Prim2("+", Expr1, Expr2)) |
       Expr MINUS Expr (Prim2("-", Expr1, Expr2)) |
       Expr TIMES Expr (Prim2("*", Expr1, Expr2)) |
       Expr DIV Expr (Prim2("/", Expr1, Expr2)) |
       Expr EQUAL Expr (Prim2("=", Expr1, Expr2)) |
       Expr DIFF Expr (Prim2("!=", Expr1, Expr2)) |
       Expr LESS Expr (Prim2("<", Expr1, Expr2)) |
       Expr LESSEQ Expr (Prim2("<=", Expr1, Expr2)) |
       Expr DOUBLECOLON Expr (Prim2("::", Expr1, Expr2)) |
       Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2)) |
       Expr LBRACK NAT RBRACK (Item(NAT, Expr))

Atomexpr: Const (Const) |
          NAME (Var(NAME)) |
          LBRACE Prog RBRACE (Prog) |
          LPAR Expr RPAR (Expr) |
          LPAR Comps RPAR (List(Comps)) |
          FN Args EQARROW Expr END (makeAnon(Args, Expr))

Appexpr : Atomexpr Atomexpr (Call(Atomexpr1, Atomexpr2)) |
          Appexpr Atomexpr (Call(Appexpr, Atomexpr))

Const : TRUE (ConB(true)) |
        FALSE (ConB(false)) |
        NAT (ConI(NAT)) |
        LPAR RPAR (List[]) |
        LPAR Type LBRACK RBRACK RPAR (ESeq(SeqT(Type)))

Comps : Expr COMMA Expr (Expr1::Expr2::[]) |
        Expr COMMA Comps (Expr::Comps)

Matchexpr : END ([]) |
            PIPE Condexpr ARROW Expr Matchexpr ((Condexpr, Expr)::Matchexpr)

Condexpr : Expr (SOME Expr) |
           UNDER (NONE)

Args : LPAR RPAR ([]) |
       LPAR Params RPAR (Params)

Params : Typedvar (Typedvar::[]) |
         Typedvar COMMA Params (Typedvar::Params)

Typedvar : Type NAME (Type, NAME)

Type : Atomtype (Atomtype) | 
       LPAR Types RPAR (ListT(Types)) |
       LBRACK Type RBRACK (SeqT(Type)) |
       Type ARROW Type (FunT(Type1, Type2))

Atomtype : NIL (ListT([])) |
           BOOL (BoolT) |
           INT (IntT) |
           LPAR Type RPAR (Type)

Types : Type COMMA Type (Type1::Type2::[]) |
        Type COMMA Types (Type::Types)
