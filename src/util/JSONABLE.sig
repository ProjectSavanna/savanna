signature JSONABLE =
  sig
    type t
    val toJSON   : t -> JSON.value
    val fromJSON : JSON.value -> t
  end
