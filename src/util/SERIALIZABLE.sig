signature SERIALIZABLE =
  sig
    type t
    val load : Filename.t -> t
    val save : Filename.t -> t -> unit
  end
