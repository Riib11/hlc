structure Main = struct

open Test;

fun main (sml_path, (image_path::args)) =
    let
	(* val _ = print (s^"\n") *)
	val _ = app (fn arg => print (arg^"\n")) args
	val _ = test "Henry"
	val _ = print "\n"
    in
	OS.Process.success
    end

end
