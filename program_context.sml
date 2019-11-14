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
	 
end
