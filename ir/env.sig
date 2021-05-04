signature ENV =
sig
    type env 
    val generate_new : unit -> env
    val insert : env * string * Temp.temp -> env
    val find : env * string -> Temp.temp option
end