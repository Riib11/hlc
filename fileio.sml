structure FileIO = struct

val readFile : string -> string =
    TextIO.input o TextIO.openIn

end
