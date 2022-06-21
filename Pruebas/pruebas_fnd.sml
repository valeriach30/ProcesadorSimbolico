(* --------------------------------------------- *)
(*Pruebas FND*)

val prufnd1 = ~:(variable "p") :&&: (variable "q") ;
val prufnd2 = (variable "q") :=>: (variable "p") ;
val prufnd3 = prufnd1 :=>: prufnd2 ;

FND prufnd3;

fun probar prop = taut (prop :<=>: (FND prop))
;

probar prufnd3;