structure Debug = struct

val DEBUG = true

fun debug tag msg : unit =
    if DEBUG then print ("["^tag^"] "^msg^"\n") else ()

val log  = debug ">"
val note = debug "*"
val warn = debug "!"

end
