structure Frame : FRAME =
struct
    val ra : Temp.temp = 0
    val sp : Temp.temp = 1
    val ret : Temp.temp = 2
    val fp : Temp.temp = 3

    val wordSize = 4
end