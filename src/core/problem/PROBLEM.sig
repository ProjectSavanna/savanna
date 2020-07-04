signature PROBLEM =
  sig
    include LOADABLE
    val grader  : t -> Filename.absolute Filename.t -> Library.t list
    val handout : t -> Filename.absolute Filename.t -> Library.t list
    val writeup : t -> Filename.relative Filename.t -> LaTeX.t
  end
