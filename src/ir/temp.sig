signature TEMP = 
sig 
    type label 
    type temp 
    val newLabel : unit -> label
    val newTemp  : unit -> temp
    val init     : unit -> unit
end
