signature LIBRARY =
  sig
    include ORDERED where type t = string

    val stage : Filename.t -> t -> Filename.t -> unit
  end
