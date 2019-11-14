structure Integer = struct

open Utilities;

exception Malformed_integer of string
				   
fun is_int (s : string) : bool =
    case String.explode s of
	[] => false
      | (s::ds) =>
	contains s Token.signs andalso
	List.all Char.isDigit ds

fun read_int (s : string) : int =
    case String.explode s of
	[] => raise (Malformed_integer s)
      | (sn::ds) =>
	let val multiplier = 
		if sn = Token.positive then 1 else
		if sn = Token.negative then ~1
		else raise (Malformed_integer s)
	    val number =
		case Int.fromString ((concat o map Char.toString) ds) of
		    SOME n => n
		  | NONE => raise (Malformed_integer s)
	in
	    multiplier * number
	end

fun toString (i : int) : string =
    if i < 0 then
	Char.toString Token.negative ^ Int.toString (abs i)
    else
	Char.toString Token.positive ^ Int.toString i
	    
end
