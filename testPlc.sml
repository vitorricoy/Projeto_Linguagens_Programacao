(* Para rodar o teste, deve-se comentar os 'use "Environ.sml";' e 'use "Absyn.sml";' de Plc.sml*)

use "testParser.sml";
use "Plc.sml";
use "testPlcCases.sml";


fun countTests (rows : (string * string) list) =
  case rows of
    [h] => 1
    |(h::t) => 1 + countTests(t);


fun testAll (rows : (string * string) list) =
let
  fun test(program : string, expected : string) = 
    if (run(fromString program)) = expected then 1 else 0
in
  case rows of
      [h] => test(h)
    | ((h:(string*string))::t) => test(h) + testAll(t)
    | [] => 0
end;

fun fails (rows : (string * string) list, failed : string list) =
let
  fun test(program : string, expected : string) = 
    if (run(fromString program)) = expected then 1 else 0;
in
  case rows of
      [h:(string*string)] => if test(h) = 1 then failed else #1h::failed
    | ((h:(string*string))::t) => if test(h) = 1 then fails(t, failed) else fails(t, #1h::failed) 
    | [] => failed
end;

val passed_tests = testAll cases;
val count_tests = countTests cases;
val failed_tests = fails(cases, []);
