structure Grammar = struct

open Utilities;
open Natural;
open Token;

type name = token

datatype program
  = Module of name * statement list
  | Executable of name * statement list * expression

and statement
    = Definition of name * expression

and expression
    = Primitive of primitive
    | Variable of name
    | Lambda of name * expression
    | Application of expression * expression
    | Binding of name * expression * expression

and primitive
    = Unit
    | Boolean of bool
    | Natural of nat
    | Integer of int

fun string_of_program (prgm : program) : string =
    case prgm of
	Module (name, stmts) =>
	unlines (
	    [ unwords [ Token.module_header, name,
			Token.assignment, Token.block_open ] ] @
	    map string_of_statement stmts @
	    [ Token.block_close ] )
      | Executable (name, stmts, main) =>
	unlines (
	    [unwords [ Token.executable_header, name,
		       Token.assignment, Token.block_open]] @
	    map string_of_statement stmts @
	    [ Token.block_close ] @
	    [ unwords [ Token.main_header, Token.assignment,
			string_of_expression main ] ] )
		
and string_of_statement (stmt : statement) : string =
    case stmt of
	Definition (name, expr) =>
	unwords (
	    [ Token.definition_header, name,
	      Token.assignment, Token.block_open,
	      string_of_expression expr,
	      Token.block_close ] )
	    
and string_of_expression (expr : expression) : string =
    case expr of
	Primitive prim =>
	string_of_primitive prim
      | Variable x => x
      | Lambda (x, e) =>
	unwords (
	    [ Token.assoc_open,
	      x, string_of_expression e,
	      Token.assoc_close ] )
      | Application (e1, e2) =>
	unwords (
	    [ Token.assoc_open,
	      string_of_expression e1,
	      string_of_expression e2,
	      Token.assoc_close ] )
      | Binding (x, e1, e2) =>
	unwords (
	    [ x, Token.assignment,
	      string_of_expression e1,
	      Token.semicolon,
	      string_of_expression e2 ] )

and string_of_primitive (prim : primitive) : string =
    case prim of
	Unit      => Token.unit
      | Boolean b => Bool.toString b
      | Natural n => Natural.toString n
      | Integer i => Integer.toString i
				      
end
