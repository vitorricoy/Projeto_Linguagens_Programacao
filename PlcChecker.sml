(* PlcChecker *)

use "Environ.sml";
use "Absyn.sml";

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (exp: expr) (state: plcType env) : plcType = 
    case exp of 
        Var(nome) => lookup state nome
      | ConI(_) => IntT
      | ConB(_) => BoolT
      | List(l) => ListT(calcularTiposList (l) (state)) (* Nil está coberto nesse caso *)

      | ESeq(t) => 
        (
            case t of
                (SeqT tt) => SeqT(tt)
                | _ => raise EmptySeq
        )
      | Let(var, e1, e2) => 
            let
                val tipo1 = teval e1 state
                val tipo2 = teval e2 ((var, tipo1)::state)
            in
                tipo2
            end
      | _ => raise UnknownType
      | Letrec(nomeFun, tipoPar, nomePar, tipoRetFun, corpoFun, ex) =>
            let
              val tipoCorpoFun = teval corpoFun ((nomeFun, FunT(tipoPar, tipoRetFun))::(nomePar, tipoPar)::state)
              val tipoEx = teval ex ((nomeFun, FunT(tipoPar, tipoRetFun))::state)
            in
              if tipoCorpoFun = tipoRetFun then
                tipoEx
              else 
                raise WrongRetType
            end
      | 

(* Declara uma função para tratar os tipos de uma lista *)
and calcularTiposList ([]) (state: plcType env) = []
  | calcularTiposList (h::t) (state: plcType env) = (teval h state)::(calcularTiposList(t) (state))