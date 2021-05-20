signature FRAME =
sig
    val ra : Temp.temp
    val sp : Temp.temp
    val fp : Temp.temp
    val ret : Temp.temp 

    val wordSize : int 
end