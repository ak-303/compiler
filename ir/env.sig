signature ENV =
sig
    type env 
    type fn_env 

    val generate_new    : unit -> env
    val generate_new_fn : unit -> fn_env

    val insert    : env * string * Temp.temp -> env
    val insert_fn : fn_env * string * Temp.label * int -> fn_env
    val addPredefined : fn_env -> fn_env

    val find : env * string -> Temp.temp option
    val find_fn : fn_env * string -> (Temp.label*int) option 

end