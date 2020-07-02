signature LOADABLE =
  sig
    type t
    val load : Filename.absolute Filename.t -> t Remote.t
  end
