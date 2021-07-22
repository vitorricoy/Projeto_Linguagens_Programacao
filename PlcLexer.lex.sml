functor PlcLexerFun(structure Tokens: PlcParser_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\034\036\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\034\032\003\003\003\003\030\003\029\028\027\026\025\023\022\021\
\\019\019\019\019\019\019\019\019\019\019\017\016\014\012\003\003\
\\003\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\011\003\010\003\008\
\\003\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\007\005\004\003\003\
\\003"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\006\006\006\000\000\000\006\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\000\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\000\000\000\000\009\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\020\020\020\020\020\020\020\020\020\020\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 67)], trans = 0},
{fin = [(N 53),(N 67)], trans = 0},
{fin = [(N 4),(N 57),(N 67)], trans = 5},
{fin = [(N 4)], trans = 5},
{fin = [(N 51),(N 67)], trans = 0},
{fin = [(N 13),(N 67)], trans = 8},
{fin = [(N 13)], trans = 8},
{fin = [(N 49),(N 67)], trans = 0},
{fin = [(N 47),(N 67)], trans = 0},
{fin = [(N 28),(N 67)], trans = 12},
{fin = [(N 60)], trans = 0},
{fin = [(N 33),(N 67)], trans = 14},
{fin = [(N 36)], trans = 0},
{fin = [(N 41),(N 67)], trans = 0},
{fin = [(N 65),(N 67)], trans = 17},
{fin = [(N 39)], trans = 0},
{fin = [(N 10),(N 67)], trans = 19},
{fin = [(N 10)], trans = 19},
{fin = [(N 26),(N 67)], trans = 0},
{fin = [(N 4),(N 67)], trans = 5},
{fin = [(N 17),(N 67)], trans = 23},
{fin = [(N 63)], trans = 0},
{fin = [(N 55),(N 67)], trans = 0},
{fin = [(N 22),(N 67)], trans = 0},
{fin = [(N 4),(N 24),(N 67)], trans = 5},
{fin = [(N 4),(N 45),(N 67)], trans = 5},
{fin = [(N 4),(N 43),(N 67)], trans = 5},
{fin = [(N 67)], trans = 30},
{fin = [(N 20)], trans = 0},
{fin = [(N 15),(N 67)], trans = 32},
{fin = [(N 31)], trans = 0},
{fin = [(N 7),(N 67)], trans = 34},
{fin = [(N 7)], trans = 34},
{fin = [(N 1),(N 4)], trans = 5}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lineNumber := !lineNumber+1; lex())
| 10 => let val yytext=yymktext() in NAT(strToInt(yytext), yypos, yypos) end
| 13 => let val yytext=yymktext() in keyword(yytext, yypos, yypos) end
| 15 => (NOT(yypos, yypos))
| 17 => (MINUS(yypos, yypos))
| 20 => (AND(yypos, yypos))
| 22 => (PLUS(yypos, yypos))
| 24 => (TIMES(yypos, yypos))
| 26 => (DIV(yypos, yypos))
| 28 => (EQUAL(yypos, yypos))
| 31 => (DIFF(yypos, yypos))
| 33 => (LESS(yypos, yypos))
| 36 => (LESSEQ(yypos, yypos))
| 39 => (DOUBLECOLON(yypos, yypos))
| 4 => (lineNumber := !lineNumber+1; lex())
| 41 => (SEMICOLON(yypos, yypos))
| 43 => (LPAR(yypos, yypos))
| 45 => (RPAR(yypos, yypos))
| 47 => (LBRACK(yypos, yypos))
| 49 => (RBRACK(yypos, yypos))
| 51 => (LBRACE(yypos, yypos))
| 53 => (RBRACE(yypos, yypos))
| 55 => (COMMA(yypos, yypos))
| 57 => (PIPE(yypos, yypos))
| 60 => (EQARROW(yypos, yypos))
| 63 => (ARROW(yypos, yypos))
| 65 => (COLON(yypos, yypos))
| 67 => let val yytext=yymktext() in error("\n***Erro de Lexer: caractere invalido *** \n"); 
     raise Fail ("Erro de Lexer: caractere invalido: " ^ yytext) end
| 7 => (lex())
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
