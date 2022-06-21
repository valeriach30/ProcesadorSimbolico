(* --------------------------------------------- *)
(*Pruebas FND*)

val prufnd1 = ~:(variable "p") :&&: (variable "q") ;
val prufnd2 = (variable "q") :=>: (variable "p") ;
val prufnd3 = pru1 :=>: pru2 ;

FND prufnd3;

fun probar prop = taut (prop :<=>: (fnd prop))
;

probar pru3;