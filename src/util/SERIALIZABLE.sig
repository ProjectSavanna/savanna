signature SERIALIZABLE =
  sig
    type t
    val fromJSON : JSON.value -> t
    val toJSON   : t -> JSON.value
  end
