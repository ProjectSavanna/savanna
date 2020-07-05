signature PROBLEM =
  sig
    structure Score : sig include MONOID SHOW end

    include LOADABLE
    val score   : t -> Score.t
    val handout : t -> Filename.absolute Filename.t -> Library.t list
    val writeup : t -> Filename.relative Filename.t -> LaTeX.Macro.t
    val grader  : t -> Filename.absolute Filename.t -> Library.t list
  end
