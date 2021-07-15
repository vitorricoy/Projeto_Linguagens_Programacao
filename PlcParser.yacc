%%

%name PlcParser

%pos int

%nonterm Prog of expr | Decl of expr | Expr of expr | 
    Atomexpr of expr | Appexpr of expr | Const of expr |
    Comps of expr list | Matchexpr of (expr option * expr) list | 
    Condexpr of expr option | Args of (pclType*String) list | 
    Params of (pclType*String) list | Typedvar of (pclType*string) |
    Type of pclType | Atomtype of plcType | Types of pclType list


%term VAR | FUN | REC | NAME of String | COLON | SEMICOLON | IF |
    THEN | ELSE | MATCH | WITH | NOT | HD | TL |
    ISE | PRINT | AND | LPAR | RPAR | RBRACK | LBRACK | 
    RBRACE | LBRACE | PLUS | MINUS | TIMES | DIV | EQUAL | 
    DIFF | LESS | LESSEQ | DOUBLECOLON | NAT of int | 
    CONST of int | FN | ARROW | END | TRUE | FALSE | COMMA |
    PIPE | UNDER | NIL | BOOL | INT | EOF | EQARROW

%right SEMICOLON
%nonassoc IF
%left ELSE
%left AND
%left EQUAL DIFF
%left LESS EQLESS
%right DOUBLECOLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HD TL ISE PRINT NAME
%left LBRACK

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr) | Decl (Decl) SEMICOLON Prog

Decl : VAR NAME EQUAL Expr |
       FUN NAME Args EQUAL Expr |
       FUN REC NAME Args COLON Type EQUAL Expr

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
       Expr COLON Expr | (Prim2(";", Expr1, Expr2)) |
       Expr LBRACK NAT RBRACK (Item(NAT, Expr))

Atomexpr: Const (Const) |
          NAME (Var(NAME)) |
          LBRACE Prog RBRACE (Prog) |
          RPAR Expr LPAR (Expr) |
          RPAR Comps LPAR (List(Comps)) |
          FN Args EQARROW Expr END (makeAnon(Args, Expr))

Appexpr : Atomexpr Atomexpr (Call(Atomexpr, Atomexpr)) |
          Appexpr Atomexpr (Call(Appexpr, Atomexpr))

Const : TRUE (ConB(true)) |
        FALSE (ConB(false)) |
        NAT (ConI(NAT)) |
        LPAR RPAR (List[]) |
        LPAR Type LBRACK RBRACK RPAR (ESeq(SeqT(Type))) |

Comps : Expr COMMA Expr (Expr; Expr) |
        Expr COMMA Comps (Expr; Comps)

Matchexpr : END | ([])
            PIPE Condexpr ARROW Expr Matchexpr ((Condexpr, Expr)::Matchexpr)

Condexpr : Expr | (SOME Expr)
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

Atomtype : NIL (ListT[]) |
           BOOL (BooT) |
           INT (IntT) |
           LPAR Type RPAR (Type)

Types : Type COMMA Type (Type1::Type2::[]) |
        Type COMMA Types (Type::Types)