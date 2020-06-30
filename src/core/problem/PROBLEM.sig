signature PROBLEM =
  sig
    include CONFIG
    val handout : t -> Filename.t -> Library.t list
  end
