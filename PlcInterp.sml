(* PlcInterp *)

use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (exp: expr) (state: plcVal env) : plcVal = 
    case exp of 
        Var(nome) => lookup state nome
      | ConI(v) => IntV(v)
      | ConB(v) => BoolV(v)
      | List(l) => ListV(calcularValoresList (l) (state)) (* Nil está coberto nesse caso *)
      | ESeq(t) => 
        (
            case t of
                (SeqT tt) => SeqV[]
                | _ => raise Impossible
        )
      | Let(var, e1, e2) => 
            let
                val val1 = eval e1 state
                val val2 = eval e2 ((var, val1)::state)
            in
                val2
            end
      | Letrec(nomeFun, tipoPar, nomePar, tipoRetFun, corpoFun, ex) =>
            eval ex ((nomeFun, Clos(nomeFun, nomePar, corpoFun, state))::state)
      | Anon(tipoArg, nomeArg, corpo) => Clos("", nomeArg, corpo, state)
      | Call(func, par) =>
          let
            val closure = eval func state
            val valPar = eval par state
          in
            case closure of 
                Clos(nomeFun, nomePar, corpoFun, state) =>
                  let
                    val novoEstado = (nomePar, valPar)::state
                  in
                    eval corpoFun novoEstado
                  end
              | _ => raise NotAFunc
          end
      | If(cond, e1, e2) => 
          let
            val valCond = eval cond state
          in
            case valCond of 
                BoolV(true) => eval e1 state
              | BoolV(false) => eval e2 state
              | _ => raise Impossible
          end
      | Match(e, matchExpr) => 
          if matchExpr = [] then
            raise Impossible
          else
            let
              val valExpr = eval e state
            in
              encontrarMatchExpr matchExpr valExpr state
            end
      | Prim1("!", e) =>
          let
            val valExpr = eval e state
          in
            case valExpr of
                BoolV(true) => BoolV(false)
              | BoolV(false) => BoolV(true)
              | _ => raise Impossible
          end
      | Prim1("-", e) =>
          let
            val valExpr = eval e state
          in
            case valExpr of
                IntV(v) => -v
              | _ => raise Impossible
          end
      | Prim1("hd", e) => 
          let
            val valExpr = eval e state
          in
            case valExpr of
                ListV([]) => raise HDEmptySeq
              | ListV(l) => hd l
              | _ => raise Impossible
          end
      | Prim1("tl", e) =>
          let
            val valExpr = eval e state
          in
            case valExpr of
                ListV([]) => raise TLEmptySeq
              | ListV(l) => tl l
              | _ => raise Impossible
      | Prim1("ise", e) =>
        
      | Prim1("print", e) =>
          
      | Prim2("&&", e1, e2) =>
         
      | Prim2("::", e1, e2) =>
          
      | Prim2(operador, e1, e2) => 
          let
            val tipoE1 = teval e1 state
            val tipoE2 = teval e2 state
          in
            (case operador of 
              "+" => 
             | "-" => 
             | "*" => 
             | "/" => 
             | "<" => 
             | "<=" => 
             | "=" => 
             | "!=" => 
             | ";" => 
             | _ => raise Impossible
            )
          end
      | Item(ind, e) => 
      | _ => raise Impossible
(* Declara uma função para tratar os valores de uma lista *)
and calcularValoresList ([]) (state: plcVal env) = []
  | calcularValoresList (h::t) (state: plcVal env) = (eval h state)::(calcularValoresList(t) (state))

(* Declara uma função para encontrar o match do valor dado *)
and encontrarMatchExpr [] valor (state: plcVal env) = 
        raise ValueNotFoundInMatch
  | encontrarMatchExpr (((SOME exp), res)::t) valor (state: plcVal env) = 
        let
          val valExp = eval exp state
        in
          if valor = valExp then
            eval res state
          else
            encontrarMatchExpr t valor state
        end
  | encontrarMatchExpr ((NONE, res)::t) valor (state: plcVal env) = 
        eval res state