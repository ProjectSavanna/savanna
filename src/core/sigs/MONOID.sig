signature MONOID =
  sig
    type t
    val z : t
    val f : t * t -> t
  end
