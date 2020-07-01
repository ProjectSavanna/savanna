signature PROBLEM =
  sig
    include LOADABLE
    val handout : t -> Filename.t -> Library.t list
    val grader  : t -> Filename.t -> Library.t list
  end
