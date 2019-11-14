structure Reducing = struct

open Utilities;
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

fun substitute e' x e =
    case e of
	Primitive    p           => Primitive p
      | Variable     y           => if x = y then e'
				    else Variable y
      | Lambda      (x1, e1)     => Lambda (
				       x1,
				       if x = x1 then e1
				       else substitute e' x e1)
      | Application (e1, e2)     => Application (
				       substitute e' x e1,
				       substitute e' x e2)
      | Binding     (x1, e1, e2) => Binding (
				       x1,
				       substitute e' x e1,
				       if x = x1 then e2 else substitute e' x e2)

(*
 * reduce
 *)

fun reduce (prgm_ctx : program_context) (e : expression) : expression reduction_result =
    let val reduce' = reduce prgm_ctx in
	case e of
	    (* |-   p normal *)
	    Primitive p => Normal $ Primitive p

	  (* |-   x normal *)
	  | Variable  x => raise Unimplemented "reduce (Variable x)"

	  (* |-   (x => e) normal *)
	  | Lambda (x, e) => Normal $ Lambda (x, e)
	  (* (case reduce' e of *)
	  (* 	 (* |-    *) *)
	  (* 	 Reduced e' => Reduced e' *)
	  (*    | Normal e => Normal $ Lambda (x, e)) *)

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
	       | Normal e1 =>
		 (case reduce' e2 of
		      (* e1 normal ; e2 ~> e2'   |-   ((e1 e2) e3) ~> ((e1 e2') e3) *)
		      Reduced e2' => Reduced $ Application (Application (e1, e2'), e3)
		    | Normal e2 =>
		      (case reduce' e3 of
			   (* e1, e2 normal ; e3 ~> e3'   |-   ((e1 e2) e3) ~> ((e1 e2) e3') *)
			   Reduced e3' => Reduced $ Application (Application (e1, e2), e3')
			 (* e1, e2, e3 normal   |-   ((e1 e2) e3) normal *)
			 | Normal e3 => Normal $ Application (Application (e1, e2), e3))))

	  | Application (e1, e2) => Normal $ Application (e1, e2)

	  | Binding (x1, e1, e2) => Reduced $ substitute e1 x1 e2
    end

fun normalize (prgm_ctx : program_context) : expression -> expression =
    let fun normalize' e =
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
