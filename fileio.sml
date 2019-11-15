structure FileIO = struct

fun readFile (file_path : string) : string =
    ((TextIO.input o TextIO.openIn) file_path)
    handle e =>
	   let val _ = print ("FileIO error; file not found: "^file_path^"\n") in
	       raise e
	   end
	       

end
