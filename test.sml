structure Test = struct

open Utilities;
open FileIO;
open Natural;
open Lexing;
open Parsing;
open Expanding;
open Interpreting;
open Reducing;

exception No_source_given;

infix $;
fun f $ x = f x

infix &;
fun x & f = f x

(*--------------------------------------------------------------*)
(* options *)

(*
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;
val _ = Control.Print.printDepth  := 1000;
*)

(*--------------------------------------------------------------*)
(* constants *)

val divider : string = "\n" ^ String.concat (replicate 50 "=") ^ "\n"
fun section hdr : unit = print (divider ^ hdr ^ "\n")

(*--------------------------------------------------------------*)
(* main *)

fun main (sml_path, (image_path::args)) =
    let
	val _ = section "[arguments]"
	val _ = print $ unlines args^"\n"
	
	val _ = section "[source]"
	val source_path = case args of
			      file_path::_ => file_path
			    | _ => raise No_source_given
	val source = readFile source_path
	val _ = print source

	val _ = section "[lexed]"
	val lexed = lex source
	val _ = print o unwords $ lexed

	val _ = section "[parsed]"
	val prgm = parse lexed
	val _ = print o string_of_program $ prgm

	val _ = section "[expanded]"
	val prgm = expandProgram prgm
	val _ = print o string_of_program $ prgm
					  
	val _ = section "[interpreted]"
	val prgm_ctx = interpret prgm
	val _ = print o Program_context.toString $ prgm_ctx

	val _ = section "[normalized]"
	val normalized = normalizeMain prgm_ctx
	val _ = (print o string_of_expression $ normalized)
		handle Unexecutable => print "unexecutable"

	val _ = print (divider ^ "\n")
    in
	OS.Process.success
    end

(*--------------------------------------------------------------*)

end
