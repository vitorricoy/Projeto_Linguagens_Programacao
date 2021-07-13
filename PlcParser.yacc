%%

%name PlcParser

%pos int

%nonterm Prog of expr | Decl of expr | Expr of expr | 
    AtomExpr of expr | AppExpr of expr | Const of expr |
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

Prog : Expr | Decl SEMICOLON Prog

Decl : VAR NAME EQUAL Expr |
       FUN NAME Args EQUAL Expr |
       FUN REC NAME Args COLON Type EQUAL Expr

Expr : AtomExpr |
       AppExpr |
       IF Expr THEN Expr ELSE Expr |
       MATCH Expr WITH Matchexpr |
       NOT Expr |
       MINUS Expr |
       HD Expr |
       TL Expr |
       ISE Expr | 
       PRINT Expr |
       Expr AND Expr |
       Expr PLUS Expr |
       Expr MINUS Expr |
       Expr TIMES Expr |
       Expr DIV Expr |
       Expr EQUAL Expr |
       Expr DIFF Expr |
       Expr LESS Expr |
       Expr LESSEQ Expr |
       Expr DOUBLECOLON Expr |
       Expr COLON Expr |
       Expr LBRACK NAT RBRACK

AtomExpr: Const |
          NAME |
          LBRACE Prog RBRACE |
          RPAR Expr LPAR |
          RPAR Comps LPAR |
          FN Args EQARROW Expr END

AppExpr : AtomExpr AtomExpr |
          AppExpr AtomExpr

Const : TRUE |
        FALSE |
        NAT |
        LPAR RPAR |
        LPAR Type LBRACK RBRACK RPAR |

Comps : Expr COMMA Expr |
        Expr COMMA Comps

Matchexpr : END |
            PIPE Condexpr ARROW Expr Matchexpr

Condexpr : Expr |
           UNDER

Args : LPAR RPAR |
       LPAR Params RPAR

Params : Typedvar |
         Typedvar COMMA Params

Typedvar : Type NAME

Type : Atomtype | 
       LPAR Types RPAR |
       LBRACK Type RBRACK |
       Type ARROW Type

Atomtype : NIL |
           BOOL |
           INT |
           LPAR Type RPAR

Types : Type COMMA Type |
        Type COMMA Types