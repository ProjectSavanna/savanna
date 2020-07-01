signature PROBLEM =
  sig
    include CONFIG
    val handout : t -> Filename.t -> Library.t list
    val grader  : t -> Filename.t -> Library.t list
  end
