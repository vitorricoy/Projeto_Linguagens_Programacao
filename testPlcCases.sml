val cases =
   (* tipos certos *)
   (
     let val s = "if 11 = 12 then 1 else 0";
         val e = "0: Int"
     in
        (s, e)
     end
   ) ::
   (
     let val s = "var b = 1 = 2; if b then 3 else 4";
         val e = "4: Int"
     in
        (s, e)
     end
   ) ::
   (
     let val s = "fun rec f1(Int x) : Int = x+1; f1(12)";
         val e = "13: Int"
     in
        (s, e)
     end
   ) :: 
   (* erros de tipo *)
   (
     let val s = "var b = 1 = 2; if b then b else 6";
         val e = "Os tipos das expressoes dos possiveis caminhos da condicao sao diferentes."
     in
        (s, e)
     end
   ) ::
   (
     let val s = "var f = fn (Bool x) => if x then 11 else 22 end; f(0)";
         val e = "A chamada da funcao recebe um tipo diferente do qual ela suporta."
     in
        (s, e)
     end
   ) :: 
   (
     let val s = "fun rec f(Bool x): Bool = if x then 11 else 22; f(true)";
         val e = "O tipo de retorno da funcao nao condiz com o seu corpo." 
     in
        (s, e)
     end
   ) :: 
   (* testes do parser *)
   ;
