structure Token = struct

type token = string
type tokens = token list

(* headers *)

val module_header     : token = "Module"
val executable_header : token = "Executable"
val program_headers   : token list =
    [module_header, executable_header]

val main_header : token = "Main"

val definition_header : token = "Definition"
val statement_headers : token list =
    [definition_header]

(* syntax *)
	
val assignment : token = ":="
val mapping    : token = "=>"
val semicolon  : token = ";"

val block_open  : token = "{"
val block_close : token = "}"

val assoc_open  : token = "("
val assoc_close : token = ")"

val expression_breaks : token list =
    [block_close, assoc_close, semicolon]

(* primitives *)
			      
val unit : token = "*"

val true_    : token = "true"
val false_   : token = "false"
val booleans : token list =
    [true_, false_]

val negative : char = #"-"
val positive : char = #"+"
val signs    : char list =
    [negative, positive]
	
end
