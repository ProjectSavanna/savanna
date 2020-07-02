signature PROBLEM =
  sig
    include LOADABLE
    val grader  : t -> Filename.t -> Library.t list
    val handout : t -> Filename.t -> Library.t list
    val writeup : t -> Filename.t -> string
  end
