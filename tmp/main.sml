(* fun main s ss =
  let val _ = print s
      val _ = app print ss
  in
    OS.Process.success
  end *)

val _ = print "hello world"

(*
structure Main =
struct

fun main s ss =
  let val _ = print s
      val _ = app print ss
  in
    OS.Process.success
  end

end
*)
