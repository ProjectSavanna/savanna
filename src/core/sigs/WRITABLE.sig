signature WRITABLE =
  sig
    type t
    val stage : t * absolute Filename.t -> unit
  end
