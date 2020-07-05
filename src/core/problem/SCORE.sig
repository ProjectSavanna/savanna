signature SCORE =
  sig
    include MONOID  (* commutative *)
    include SHOW
    val fromJSON : JSON.value -> t
  end
