signature LIBRARY =
  sig
    include ORDERED where type t = string

    val stage :
      Filename.absolute Filename.t
      -> t -> Filename.absolute Filename.t
      -> unit
  end
