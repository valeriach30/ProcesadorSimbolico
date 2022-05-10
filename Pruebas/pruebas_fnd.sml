(* --------------------------------------------- *)
(*Pruebas FND*)

val prufnd1 = ~:(variable "p") :&&: (variable "q") ;
val prufnd2 = (variable "q") :=>: (variable "p") ;
val prufnd3 = pru1 :=>: pru2 ;

FND prufnd3;

val prufnd4 = (variable "a") :=>: (variable "b") ;
FND prufnd4;

val prufnd5 = (variable "a") :<=>: (variable "b") ;
FND prufnd5;