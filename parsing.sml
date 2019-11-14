structure Parsing = struct

open Debug;
open Integer;
open Natural;
open Lexing;
open Grammar;
open Token;

(*
 * parsing exceptions
 *)

exception Parse_error;
exception Unparsed of tokens;
exception Unexpected_end;
exception Expected of token * tokens;

(*
 * parse
 *)

fun parse (ts : tokens) : program =
    (case parseProgram ts of
  	   (prgm, []) => prgm
  	 | (_, ts) => raise (Unparsed ts))
    handle
      Unparsed ts =>
        let val _ = String.concat
  		      [ "Parse Error: Unparsed;\n"
  		      , "some tokens were left unparsed at the end of the source.\n"
  		      , "..."^unwords ts^"\n" ]
        in raise Parse_error end
    | Unexpected_end =>
        let val _ = String.concat
            [ "Parse error: Unexpected_end;\n"
            , "the end of the source was reached when a token was expected\n"
            , "..."^unwords ts^"\n" ]
        in raise Parse_error end
    | Expected (t, ts) =>
        let val _ = String.concat
            [ "Parse error: Expected '"^t^"'\n"
            , "..."^unwords ts^"\n" ]
        in raise Parse_error end

(*
 * parse program
 *)

and parseProgram (ts : tokens) : program * tokens =
    if checkToken Token.module_header ts then
	let val _           = log "parsing module"
	    val (_,     ts) = parseToken Token.module_header ts
	    val (name,  ts) = parseName ts
	    val (_,     ts) = parseToken Token.assignment ts
	    val (stmts, ts) = parseStatementsBlock ts
	in
	    (Module (name, stmts), ts)
	end
    else if checkToken Token.executable_header ts then
	let val _           = log "parsing executable"
	    val (_,     ts) = parseToken Token.executable_header ts
	    val (name,  ts) = parseName ts
	    val (_,     ts) = parseToken Token.assignment ts
	    val (stmts, ts) = parseStatementsBlock ts
	    val (main,  ts) = parseMain ts
	in
	    (Executable (name, stmts, main), ts)
	end
    else
	raise (Expected ("program header", ts))

and parseMain (ts : tokens) : expression * tokens =
    let val _          = log "parsing main"
	val (_,    ts) = parseToken Token.main_header ts
	val (_,    ts) = parseToken Token.assignment ts
	val (expr, ts) = parseExpressionBlock ts
    in
	(expr, ts)
    end


(*
 * parse statement
 *)

and parseStatementsBlock (ts : tokens) : statement list * tokens =
    let fun parseStatementBlock' ts =
	    if checkToken Token.block_close ts then
		([], ts)
	    else
		let val (stmt,  ts) = parseStatement ts
		    val (stmts, ts) = parseStatementBlock' ts
		in
		    (stmt :: stmts, ts)
		end
    in
	let val _           = log "parsing statement block"
	    val (_,     ts) = parseToken Token.block_open ts
	    val (stmts, ts) = parseStatementBlock' ts
	    val (_,     ts) = parseToken Token.block_close ts
	in
	    (stmts, ts)
	end
    end

and parseStatement (ts : tokens) : statement * tokens =
    if checkToken Token.definition_header ts then
	let val _          = log "parsing definition"
	    val (_,    ts) = parseToken Token.definition_header ts
	    val (name, ts) = parseName ts
	    val (_,    ts) = parseToken Token.assignment ts
	    val (expr, ts) = parseExpressionBlock ts
	in
	    (Definition (name, expr), ts)
	end
    else
	raise (Expected ("statement header", ts))

(*
 * parse expression
 *)

and parseExpressionBlock (ts : tokens) : expression * tokens =
    let val _          = log "parsing expression block"
	val (_,    ts) = parseToken Token.block_open ts
	val (expr, ts) = parseExpression ts
	val (_,    ts) = parseToken Token.block_close ts
    in
	(expr, ts)
    end

and parseExpression (ts : tokens) : expression * tokens =
    let fun parseExpression' (ts : tokens) : expression * tokens =
	    if checkToken Token.assoc_open ts then
		let val _          = log "parsing association open"
                    val (_,    ts) = parseToken Token.assoc_open ts
                    val (expr, ts) = parseExpression ts
                    val _          = log "parsing association close"
                    val (_,    ts) = parseToken Token.assoc_close ts
		in
                    (expr, ts)
		end
	    else if checkNToken 1 Token.mapping ts then
		let val _          = log "parsing lambda"
		    val (name, ts) = parseName ts
		    val (_,    ts) = parseToken Token.mapping ts
		    val (expr, ts) = parseExpression ts
		in
		    (Lambda (name, expr), ts)
		end
	    else if checkNToken 1 Token.assignment ts then
		let val _           = log "parsing binding"
		    val (name,  ts) = parseName ts
		    val (_,     ts) = parseToken Token.assignment ts
		    val (expr1, ts) = parseExpression ts
		    val (_,     ts) = parseToken Token.semicolon ts
		    val (expr2, ts) = parseExpression ts
		in
		    (Binding (name, expr1, expr2), ts)
		end
	    else if checkPrimitive ts then
		let val _          = log "parsing primitive"
		    val (prim, ts) = parsePrimitive ts in
		    (Primitive prim, ts)
		end
	    else
		let val _          = log "parsing variable"
		    val (name, ts) = parseName ts in
		    (Variable name, ts)
		end
	val (expr, ts) = parseExpression' ts
    in
	if checkTokenFrom Token.expression_breaks ts then
	    let val _ = log "checked expression break"
	    in
		(expr, ts)
	    end
	else
	    let val _           = log "parsing application"
		val (expr', ts) = parseExpression ts
	    in
		(Application (expr, expr'), ts)
	    end
    end

(*
 * parse primitive
 *)

and parsePrimitive (ts : tokens) : primitive * tokens =
    if checkUnit ts then
	let val _       = log "parsing unit"
	    val (_, ts) = parseNext ts in
	    (Unit, ts)
	end
    else if checkBoolean ts then
	let val _       = log "parsing boolean"
	    val (t, ts) = parseNext ts
	in
	    if t = Token.true_ then (Boolean true, ts) else
	    if t = Token.false_ then (Boolean false, ts)
	    else raise (Expected ("boolean", ts))
	end
    else if checkNatural ts then
	let val _       = log "parsing natural"
	    val (t, ts) = parseNext ts in
	    (Natural (read_nat t), ts)
	end
    else if checkInteger ts then
	let val _       = log "parsing integer"
	    val (t, ts) = parseNext ts in
	    (Integer (read_int t), ts)
	end
    else
	raise (Expected ("primitive", ts))

(*
 * parse name
 *)

and parseName ts : token * tokens =
    let val (name, ts) = parseNext ts
	val _          = log ("parsing name '"^name^"'")
    in
	(name, ts)
    end

(*
 * check primitive
 *)

and checkPrimitive ts : bool =
    checkFrom [ checkUnit, checkBoolean, checkNatural ] ts

and checkUnit    ts : bool = checkToken Token.unit ts
and checkBoolean ts : bool = checkTokenFrom Token.booleans ts
and checkNatural ts : bool = check is_nat ts
and checkInteger ts : bool = check is_int ts

(*
 * parsing utilities
 *)

and parseToken (t : token) ts : token * tokens =
    if checkToken t ts then
	let val _ = log ("parsing token '"^t^"'") in
	    parseNext ts
	end
    else
	raise (Expected (t, ts))

and parseNext ts : token * tokens =
    case ts of
	[] => raise Unexpected_end
      | (t::ts') => (t, ts')

(*
 * checking utilities
 *)

and checkN (n : int) (c : token -> bool) ts : bool =
    case List.drop (ts, n) of
	[] => false
      | (t'::_) => c t'

and check (c : token -> bool) ts : bool =
    checkN 0 c ts

and checkFrom (chks : (tokens -> bool) list) ts : bool =
    List.exists (fn chk => chk ts) chks

and checkNToken (n : int) (t : token) ts : bool=
    checkN n (fn t' => t = t') ts

and checkToken (t : token) ts : bool =
    checkNToken 0 t ts

and checkTokenFrom (ts' : tokens) ts : bool =
    List.exists (flip checkToken ts) ts'

end
