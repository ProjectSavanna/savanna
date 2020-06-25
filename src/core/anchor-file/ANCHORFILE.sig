signature ANCHORFILE =
  sig
    include WRITABLE
    val fromPath : Filename.t * Filename.t -> t Remote.t
  end
