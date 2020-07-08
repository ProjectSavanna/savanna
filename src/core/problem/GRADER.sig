signature GRADER =
  sig
    structure Score : SCORE

    include LOADABLE
    val score : t -> Score.t
    val combine : t list -> t

    val build : t -> Filename.absolute Filename.t -> LibrarySet.set
  end
