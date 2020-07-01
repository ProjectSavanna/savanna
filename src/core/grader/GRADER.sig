signature GRADER =
  sig
    include LOADABLE
    val stage : t -> Filename.t -> Library.t list
  end
