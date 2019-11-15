structure Program_context = struct

open Grammar

type program_context =
     { name : name
     , definitions : (name * expression) list
     , main : expression option }

val empty_program_context : program_context =
    { name = ""
    , definitions = []
    , main = NONE }

fun toString (prgm_ctx : program_context) : string =
    unlines (
	(map (fn (n, e) =>
		 unwords [ n,
			   Token.assignment,
			   Token.block_open,
			   string_of_expression e,
			   Token.block_close ])
	     (#definitions prgm_ctx)) @
	(case #main prgm_ctx of
	     SOME main =>
	     [ unwords
		   [ Token.main_header,
		     Token.block_open,
		     string_of_expression main,
		     Token.block_close ] ]
	   | NONE => [] ))
 	
fun getAssignment (prgm_ctx : program_context) (x : string) : expression option =
    case List.find (fn (y, e) => x = y) (#definitions prgm_ctx) of
	SOME (_, e) => SOME e
      | NONE => NONE
		    
end
