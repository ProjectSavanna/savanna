signature RUBRIC =
  sig
    type 'a item = {
      grader      : 'a,
      points      : int,
      description : string
    }
    type 'a t = 'a item list

    val toJSON : ('a -> JSON.value) -> 'a t -> JSON.value
    val fromJSON : (JSON.value -> 'a) -> JSON.value -> 'a t
  end
