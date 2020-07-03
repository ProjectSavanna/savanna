signature ASSIGNMENT =
  sig
    include LOADABLE
    val grader  : t -> Filename.absolute Filename.t -> unit
    val handout : t -> Filename.absolute Filename.t -> unit
    val writeup : t -> string -> Filename.absolute Filename.t -> unit
  end
