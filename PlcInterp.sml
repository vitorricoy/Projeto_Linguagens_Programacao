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