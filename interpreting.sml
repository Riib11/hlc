structure Interpreting = struct

open Utilities;
open Grammar;
open Program_context;

(*
 * interpret
 *)

val interpretStatements : statement list -> (name * expression) list =
    let fun interpretStatement stmt =
	    case stmt of
		Definition (name, expr) => (name, expr)
    in
	map interpretStatement
    end
	
val interpret : program -> program_context =
    let fun interpret' (ctx : program_context) (p : program) : program_context =
	    case p of
		Module (name, stmts) =>
		{ name        = name
		, definitions = interpretStatements stmts
		, main        = NONE }
	      | Executable (name, stmts, main) =>
		{ name        = name
		, definitions = interpretStatements stmts
		, main        = SOME main }
	      | Main main =>
		{ name        = "Main"
		, definitions = []
		, main        = SOME main }
    in
	interpret' empty_program_context
    end
	
end
