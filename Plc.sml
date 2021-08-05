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
          SymbolNotFound => "Variável não definida."
        (* Exceções do PlcChecker.sml *)
        | EmptySeq => "A sequência não possui elementos."
        | UnknownType => "Tipo inválido utilizado."
        | NotEqTypes => "Tipos diferentes usados na comparação."
        | WrongRetType => "O tipo de retorno da função não condiz com o seu corpo."
        | DiffBrTypes => "Os tipos das expressões dos possíveis caminhos da condição são diferentes."
        | IfCondNotBool => "Condicional possui uma condição não booleana."
        | NoMatchResults => "Não há resultados para a expressão match."
        | MatchResTypeDiff => "O tipo de algum dos casos de uma expressão match difere dos demais."
        | MatchCondTypesDiff => "O tipo das opções de match diferem do tipo da expressão passada para o match."
        | CallTypeMisM => "A chamada da função recebe um tipo diferente do qual ela suporta."
        | NotFunc => "A expressão chamada não é uma função."
        | ListOutOfRange => "Tentativa de acessar um elemento fora dos limites da lista."
        | OpNonList => "Tentativa de acessar um elemento de uma expressão que não é uma lista."
        (* Exceções do PlcInterp.sml *)
        | Impossible => "Erro ao avaliar a expressão."
        | HDEmptySeq => "hd foi utilizado em uma sequência vazia."
        | TLEmptySeq => "tl foi utilizado em uma sequência vazia."
        | ValueNotFoundInMatch => "Valor da expressão não foi encontrado no match."
        | NotAFunc => "Tentativa de usar como função uma expressão que não é uma função.";
