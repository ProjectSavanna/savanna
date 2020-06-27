signature LIBRARY =
  sig
    include ORDERED where type t = {
      name : string,
      root : Filename.t
    }

    val fromName : string -> t
    val stage : t * Filename.t -> unit
  end
