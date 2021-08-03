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
      | Anon(tipoArg, nomeArg, corpo) => 
          let
            val tipoCorpo = teval corpo ((nomeArg, tipoArg)::state)
          in
            FunT(tipoArg, tipoCorpo)
          end
      | Call(func, par) =>
          let
            val tipoFunc = teval func state
            val tipoPar = teval par state
          in
            case tipoFunc of 
                FunT(t1, t2) =>
                  if t1 = tipoPar then
                    t2
                  else
                    raise CallTypeMisM
              | _ => raise NotFunc
          end
      | If(cond, e1, e2) => 
          let
            val tipoCond = teval cond state
            val tipoE1 = teval e1 state
            val tipoE2 = teval e2 state
          in
            if tipoCond = BoolT then
              if tipoE1 = tipoE2 then
                tipoE1
              else
                raise DiffBrTypes
            else
              raise IfCondNotBool
          end
      | Match(e, matchExpr) => 
          if matchExpr = [] then
            raise NoMatchResults
          else
            let
              val tipoExpr = teval e state
              val tipoRetorno = teval (#2(hd matchExpr)) state
            in
              if verificarTiposCondMatchExpr(matchExpr, tipoExpr) state then
                verificarTiposRetMatchExpr(matchExpr, tipoRetorno) state
              else
                raise MatchCondTypesDiff
            end
      | Prim1("!", e) => if teval e state = BoolT then BoolT else raise UnknownType
      | Prim1("-", e) => if teval e state = IntT then IntT else raise UnknownType
      | Prim1("hd", e) => 
          let
            val tipoE = teval e state
          in
            retornarTipoSeqT tipoE
          end
      | Prim1("tl", e) =>
          let
            val tipoE = teval e state
          in
            SeqT(retornarTipoSeqT tipoE)
          end
      | Prim1("ise", e) =>
        let
          val tipoE = teval e state
          val retorno = retornarTipoSeqT(tipoE)
        in
          BoolT (* Se existir um erro com 'e' a exceção é disparada na função auxiliar *)
        end
      | Prim1("print", e) =>
          let
            val tipoE = teval e state
          in
            ListT[]
          end
      | Prim2("&&", e1, e2) =>
          let
            val tipoE1 = teval e1 state
            val tipoE2 = teval e2 state
          in
            if tipoE1 = BoolT andalso tipoE2 = BoolT then
              BoolT
            else
              raise UnknownType
          end
      | Prim2("::", e1, e2) =>
          let
            val tipoE1 = teval e1 state
            val tipoE2 = teval e2 state
          in
            if tipoE2 = SeqT(tipoE1) then
              tipoE2
            else
              raise UnknownType
          end
      | Prim2(operador, e1, e2) => 
          let
            val tipoE1 = teval e1 state
            val tipoE2 = teval e2 state
          in
            (case operador of 
              "+" => if tipoE1 = IntT andalso tipoE2 = IntT then 
                        IntT 
                       else 
                        raise UnknownType
             | "-" => if tipoE1 = IntT andalso tipoE2 = IntT then 
                        IntT 
                       else 
                        raise UnknownType
             | "*" => if tipoE1 = IntT andalso tipoE2 = IntT then 
                        IntT 
                       else 
                        raise UnknownType
             | "/" => if tipoE1 = IntT andalso tipoE2 = IntT then 
                        IntT 
                       else 
                        raise UnknownType
             | "<" => if tipoE1 = IntT andalso tipoE2 = IntT then 
                        BoolT 
                       else 
                        if tipoE1 = tipoE2 then raise UnknownType else raise NotEqTypes
             | "<=" => if tipoE1 = IntT andalso tipoE2 = IntT then 
                          BoolT 
                        else 
                          if tipoE1 = tipoE2 then raise UnknownType else raise NotEqTypes
             | "=" => if tipoE1 = tipoE2 andalso verificarEqualityType tipoE1 then
                        BoolT
                       else 
                        if tipoE1 = tipoE2 then raise UnknownType else raise NotEqTypes
             | "!=" => if tipoE1 = tipoE2 andalso verificarEqualityType tipoE1 then
                        BoolT
                      else 
                        if tipoE1 = tipoE2 then raise UnknownType else raise NotEqTypes
             | ";" => tipoE2
             | _ => raise UnknownType
            )
          end
      | Item(ind, e) => 
          let
            val tipoE = teval e state
          in
            (case tipoE of
                ListT(l) => if ind = 0 then 
                              raise ListOutOfRange 
                            else 
                              calculaTipoAcessoSequencia(ind, l)
              | _ => raise OpNonList)
          end
      | _ => raise UnknownType

(* Declara uma função para tratar os tipos de uma lista *)
and calcularTiposList ([]) (state: plcType env) = []
  | calcularTiposList (h::t) (state: plcType env) = (teval h state)::(calcularTiposList(t) (state))

(* Declara uma função para verificar os tipos das condições de uma expressão
   de match, considerando que o caso vazio já foi tratado *)
and verificarTiposCondMatchExpr([], tCond) (state: plcType env) = true
  | verificarTiposCondMatchExpr((cond, ret)::t, tCond) (state: plcType env) = 
      case cond of 
          (SOME c) => 
            let
              val tipoCond = teval c state
            in
              if tipoCond = tCond then
                verificarTiposCondMatchExpr (t, tCond) state
              else
                raise MatchCondTypesDiff
            end
        | (NONE) => verificarTiposCondMatchExpr (t, tCond) state

(* Declara uma função para verificar os tipos dos retornos de uma expressão
   de match, considerando que o caso vazio já foi tratado *)
and verificarTiposRetMatchExpr([], tRet) (state: plcType env) = tRet
  | verificarTiposRetMatchExpr((cond, ret)::t, tRet) (state: plcType env) = 
        let
          val tipoRet = teval ret state
        in
          if tipoRet = tRet then
            verificarTiposRetMatchExpr (t, tRet) state
          else
            raise MatchResTypeDiff
        end

(* Declara uma função para retornar o tipo contido dos elementos um tipo SeqT *)
and retornarTipoSeqT(SeqT t) = t
  | retornarTipoSeqT(_) = raise UnknownType

(* Declara uma função para retornar se um tipo é um equality type *)
and verificarEqualityType(t) = 
    case t of 
        BoolT => true
      | IntT => true
      | ListT(l) =>
        let
          fun aux([]) = true
            | aux(h::t) = if verificarEqualityType(h) then verificarEqualityType(ListT(t)) else false
        in
          aux l
        end
      | (SeqT t) => verificarEqualityType t
      | _ => false

(* Declara uma função para calcular o tipo do elemento com o índice dado na lista dada *)
and calculaTipoAcessoSequencia(indice, []) = raise ListOutOfRange
  | calculaTipoAcessoSequencia(1, h::t) = h
  | calculaTipoAcessoSequencia(indice, h::t) = calculaTipoAcessoSequencia(indice-1, t)
    