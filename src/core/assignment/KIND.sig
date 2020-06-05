signature KIND =
  sig
    datatype t = Hw | Lab

    val toString : t -> string
    val fromString : string -> t option
    val fromJSON : JSON.value -> t
  end
