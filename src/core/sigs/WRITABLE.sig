signature WRITABLE =
  sig
    type t
    val stage : t * Filename.t -> unit
  end
