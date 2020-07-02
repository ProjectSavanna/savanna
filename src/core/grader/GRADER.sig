signature GRADER =
  sig
    structure Score : sig include MONOID SHOW end

    include LOADABLE
    val stage : t -> Filename.t -> Library.t list
    val tasks : t -> (string * Score.t) list
  end
