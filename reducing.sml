structure Reducing = struct

open Utilities;
open Debug;
open Grammar;
open Program_context;

infix $;
fun f $ x = f x

infix &;
fun x & f = f x

exception Unexecutable;
	      
datatype 'a reduction_result = Normal of 'a | Reduced of 'a
			   
(*
 * substitute
 *)

val fresh_id_counter : int ref = ref 0;
fun make_fresh (x : name) : name =
    let
	val id = !fresh_id_counter
	val _  = fresh_id_counter := id + 1
    in
	(* I decided to name fresh variables like this because,
	 * since the "\"" character cannot appear in programs
	 * (it is reserved as the comment delimeter), there is
	 * no possible way for the fresh name to overlap with
	 * any other variable name. *)
	x^"\""^Int.toString id^"\""
    end

fun substitute e' x e =
    case e of
	Primitive    p           => Primitive p
      | Variable     y           => if x = y then e'
				    else Variable y
      | Lambda      (x1, e2)     => let val x1' = make_fresh x1 in
					(* [e'/x] (x1 => x2) := 
					 * (x1' => [e'/x] ([x1'/x1] e2) *)
					Lambda (
					    x1',
					    substitute e' x (
						substitute (Variable x1') x1 e2))
				    end
      | Application (e1, e2)     => Application (
				       substitute e' x e1,
				       substitute e' x e2)

(*
 * reduce
 *)

fun reduce (prgm_ctx : program_context) (e : expression) : expression reduction_result =
    let val reduce' = reduce prgm_ctx
	val result =
	    case e of
		(* |-   p normal *)
		Primitive p => Normal $ Primitive p

	      | Variable x =>
		(case getAssignment prgm_ctx x of
		     (* x:=e'   |-   x ~> e' *)
		     SOME e' => Reduced e'
		   (* ~(x:=e')   |-   x normal *)
		   | NONE => Normal $ Variable x)

	      (* |-   (x => e) normal *)
	      (* | Lambda (x, e) => Normal $ Lambda (x, e) *)
	      | Lambda (x, e) =>
		(case reduce' e of
	      	     (* |-    *)
	      	     Reduced e' => Reduced e'
		   | Normal e => Normal $ Lambda (x, e))

	      | Application (Primitive p, e) =>
		(case reduce' e of
		     (* e ~> e'   |-   (p e) ~> (p e') *)
		     Reduced e' => Reduced $ Application (Primitive p, e')
		   (* e normal   |-   (p e) normal *)
		   | Normal e => Normal $ Application (Primitive p, e))

	      | Application (Variable x, e) =>
		(case reduce' e of
		     (* e ~> e'   |-   (x e) ~> (x e') *)
		     Reduced e' => Reduced $ Application (Variable x, e')
		   (* e normal   |-   (x e) normal *)
		   | Normal e => Normal $ Application (Variable x, e)) 

	      (* |-   ((x => e1) e2) ~> ([e2/x] e2) *)
	      | Application (Lambda (x, e1), e2) => Reduced $ substitute e2 x e1

	      | Application (Application (e1, e2), e3) =>
		(case reduce' e1 of
		     (* e1 ~> e1'   |-   ((e1 e2) e3) ~> ((e1' e2) e3) *)
		     Reduced e1' => Reduced $ Application (Application (e1', e2), e3)
		   | Normal  e1  =>
		     (case reduce' e2 of
			  (* e1 normal ; e2 ~> e2'   |-   ((e1 e2) e3) ~> ((e1 e2') e3) *)
			  Reduced e2' => Reduced $ Application (Application (e1, e2'), e3)
			| Normal  e2  =>
			  (case reduce' e3 of
			       (* e1, e2 normal ; e3 ~> e3'   |-   ((e1 e2) e3) ~> ((e1 e2) e3') *)
			       Reduced e3' => Reduced $ Application (Application (e1, e2), e3')
			     (* e1, e2, e3 normal   |-   ((e1 e2) e3) normal *)
			     | Normal  e3  => Normal $ Application (Application (e1, e2), e3))))

	      (* | Application (e1, e2) => Normal $ Application (e1, e2) *)

	val _ = case result of
		    Reduced e' => log $ "reduce: "^string_of_expression e^"\n"^
				  "         ~> "^string_of_expression e'
		  | Normal  e  => log $ "normal: "^string_of_expression e
    in
	result
    end

fun normalize (prgm_ctx : program_context) : expression -> expression =
    let
	fun normalize' e =
	    case reduce prgm_ctx e of
		Reduced e' => normalize' e'
	      | Normal  e  => e
    in
	normalize'
    end

fun normalizeMain (prgm_ctx : program_context) : expression =
    case #main prgm_ctx of
	SOME main => normalize prgm_ctx main
      | NONE      => raise Unexecutable

end
