(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

fun strToInt s = 
    case Int.fromString s of
        SOME i => i
      | NONE => raise Fail("Erro ao converter string '"^s^"' para int")

fun keyword (s, lpos, rpos) = 
    case s of
        "Bool" => BOOL(lpos, rpos)
        | "else" => ELSE(lpos, rpos)
        | "end" => END(lpos, rpos)
        | "false" => FALSE(lpos, rpos)
        | "fn" => FN(lpos, rpos)
        | "fun" => FUN(lpos, rpos)
        | "hd" => HD(lpos, rpos)
        | "if" => IF(lpos, rpos)
        | "Int" => INT(lpos, rpos)
        | "ise" => ISE(lpos, rpos)
        | "match" => MATCH(lpos, rpos)
        | "Nil" => NIL(lpos, rpos)
        | "print" => PRINT(lpos, rpos)
        | "rec" => REC(lpos, rpos)
        | "then" => THEN(lpos, rpos)
        | "tl" => TL(lpos, rpos)
        | "true" => TRUE(lpos, rpos)
        | "var" => VAR(lpos, rpos)
        | "with" => WITH(lpos, rpos)
        | "_" => UNDER(lpos, rpos)
        | _ => NAME(s, lpos, rpos)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit = [0-9];
whitespace = [\ \t];
identifier = [a-zA-Z_][a-zA-Z_0-9]*;
%%

\n => (lineNumber := !lineNumber+1; lex());
{whitespace}+ => (lex());
{digit}+ => (NAT(strToInt(yytext), yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));
"!" => (NOT(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"&&" => (AND(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"*" => (TIMES(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQUAL(yypos, yypos));
"!=" => (DIFF(yypos, yypos));
"<" => (LESS(yypos, yypos));
"<=" => (LESSEQ(yypos, yypos));
"::" => (DOUBLECOLON(yypos, yypos));
";" => (SEMICOLON(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"[" => (LBRACK(yypos, yypos));
"]" => (RBRACK(yypos, yypos));
"{" => (LBRACE(yypos, yypos));
"}" => (RBRACE(yypos, yypos));
"," => (COMMA(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"=>" => (EQARROW(yypos, yypos));
"->" => (ARROW(yypos, yypos));
":" => (COLON(yypos, yypos));
. =>(error("\n***Erro de Lexer: caractere invalido *** \n"); 
     raise Fail ("Erro de Lexer: caractere invalido: " ^ yytext));

