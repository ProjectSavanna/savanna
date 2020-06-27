signature LOADABLE =
  sig
    type t
    val load : Filename.t -> t Remote.t
  end
