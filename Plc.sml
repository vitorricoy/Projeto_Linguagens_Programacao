(* Plc interpreter main file *)

use "Environ.sml";
use "Absyn.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";

fun run e = 
    let
      val tipo = type2string(teval e [])
      val valor = val2string(eval e [])
    in
      valor^": "^tipo
    end
    handle
        (* Exceções do Environ.sml *)
          SymbolNotFound => "Variavel nao definida."
        (* Exceções do PlcChecker.sml *)
        | EmptySeq => "A sequência nao possui elementos."
        | UnknownType => "Tipo invalido utilizado."
        | NotEqTypes => "Tipos diferentes usados na comparacao."
        | WrongRetType => "O tipo de retorno da funcao nao condiz com o seu corpo."
        | DiffBrTypes => "Os tipos das expressoes dos possiveis caminhos da condicao sao diferentes."
        | IfCondNotBool => "Condicional possui uma condicao nao booleana."
        | NoMatchResults => "Nao ha resultados para a expressao match."
        | MatchResTypeDiff => "O tipo de algum dos casos de uma expressao match difere dos demais."
        | MatchCondTypesDiff => "O tipo das opcoes de match diferem do tipo da expressao passada para o match."
        | CallTypeMisM => "A chamada da funcao recebe um tipo diferente do qual ela suporta."
        | NotFunc => "A expressao chamada nao é uma funcao."
        | ListOutOfRange => "Tentativa de acessar um elemento fora dos limites da lista."
        | OpNonList => "Tentativa de acessar um elemento de uma expressao que nao é uma lista."
        (* Exceções do PlcInterp.sml *)
        | Impossible => "Erro ao avaliar a expressao."
        | HDEmptySeq => "hd foi utilizado em uma sequência vazia."
        | TLEmptySeq => "tl foi utilizado em uma sequência vazia."
        | ValueNotFoundInMatch => "Valor da expressao nao foi encontrado no match."
        | NotAFunc => "Tentativa de usar como funcao uma expressao que nao é uma funcao.";
