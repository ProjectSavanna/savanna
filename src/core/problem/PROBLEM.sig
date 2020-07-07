signature PROBLEM =
  sig
    structure Score : SCORE

    include LOADABLE
    val score   : t -> Score.t
    val handout : t -> Filename.absolute Filename.t -> LibrarySet.set
    val writeup : t -> Filename.relative Filename.t -> LaTeX.Macro.t
    val grader  : t -> Filename.absolute Filename.t -> LibrarySet.set
  end
