signature PROBLEM =
  sig
    include CONFIG
    val handout : t list -> Filename.t -> unit
  end
