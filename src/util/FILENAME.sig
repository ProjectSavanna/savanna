signature FILENAME =
  sig
    type 'a t

    type relative
    val relative : string -> relative t option
    val `        : string -> relative t

    val concat : 'a t * relative t -> 'a t

    type absolute
    val absolute : string -> absolute t option
    val getDir   : unit -> absolute t
    val $        : string -> absolute t

    val toString : 'a t -> string
  end
