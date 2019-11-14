exception E1;
exception E2;
exception E3;

fun main () : unit =
  if true then raise E2 else print "0\n";

val _ =
  main ()
  handle
    E1 => print "1\n"
  | E2 => print "2\n"
  | E3 => print "3\n"
