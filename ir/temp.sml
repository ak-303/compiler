structure Temp =
struct

    type label = int
    type temp  = int

    val nextLabel : label ref = ref 0
    val nextTemp  : temp ref  = ref 0

    fun newLabel () = (nextLabel := (!nextLabel) + 1; !nextLabel)
    fun newTemp  () = (nextTemp  := (!nextTemp)  + 1; !nextTemp )

    fun init () = (nextLabel := 0; nextTemp := 0; ())
end 