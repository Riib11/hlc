structure Expanding = struct

open Utilities;
open Grammar;

(* Some grammatical structures are superfluous,
 * that is, they can be entirely translated into
 * a subset of the grammar with out them.
 * The 'expand' function implements this translation,
 * yielding an expression containing only the core
 * grammar. In the future I plan on just having a
 * separate datatype called 'core_expression' for
 * this, so I can avoid "nonexhaustive pattern matching"
 * warnings with functions that expect an expanded
 * expression. That will also make it much easier
 * to add things like syntactical sugar to the
 * directly-parsed grammar. *)
fun expand (expr : expression) : expression =
    case expr of
        Primitive p => Primitive p
      | Variable x => Variable x
      | Lambda (x, e) => Lambda (x, expand e)
      | Application (e1, e2) => Application (expand e1, expand e2)
      | Binding (x1, e1, e2) => Application (Lambda (x1, expand e2), expand e1)

fun expandProgram (prgm : program) : program =
    case prgm of
	Module (name, stmts) =>
	Module (name, map expandStatement stmts)
      | Executable (name, stmts, main) =>
	Executable (name, map expandStatement stmts, expand main)
      | Main main =>
	Main (expand main)

and expandStatement (stmt : statement) : statement =
    case stmt of
	Definition (name, expr) =>
	Definition (name, expand expr)
		    
end
