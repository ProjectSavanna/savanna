signature LOADABLE =
  sig
    type t
    val fromJSON : Filename.t -> t Remote.t
  end
