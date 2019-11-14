structure Test = struct

open FileIO;
open Natural;
open Lexing;
open Parsing;
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
	val _ = section "[source]"
	val source_path = "examples/tmp.hlc"
	val source = readFile source_path
	val _ = print source
	(* val source = case CommandLine.arguments () of *)
	(* [file_path] => readFile file_path *)
	(* | _ => raise No_source_given; *)

	val _ = section "[lexed]"
	val lexed = lex source
	val _ = print o unwords $ lexed

	val _ = section "[parsed]"
	val parsed = parse lexed
	val _ = print o string_of_program $ parsed

	val _ = section "[interpreted]"
	val prgm_ctx = interpret parsed

	(* val _ = section "[reducing]"; *)
	(* val reduced = reduceMain prg_ctx; *)

	val _ = print (divider ^ "\n")
    in
	OS.Process.success
    end

(*--------------------------------------------------------------*)

end
