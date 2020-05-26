structure Rubric =
  struct
    type 'a item = {
      grader      : 'a,
      points      : int,
      description : string
    }
    type 'a t = 'a item list

    local
      val itemToJSON : ('a -> JSON.value) -> 'a item -> JSON.value =
        fn f =>
          fn {grader,points,description} =>
            JSON.OBJECT [
              ("grader"     , f grader                        ),
              ("points"     , JSON.INT (IntInf.fromInt points)),
              ("description", JSON.STRING description         )
            ]
    in
      val toJSON : ('a -> JSON.value) -> 'a t -> JSON.value = fn f => JSON.ARRAY o List.map (itemToJSON f)
    end

    local
      val itemFromJSON : (JSON.value -> 'a) -> JSON.value -> 'a item = fn f => fn
        JSON.OBJECT [
          ("grader"     , grader                  ),
          ("points"     , JSON.INT points         ),
          ("description", JSON.STRING description )
        ] => {
          grader      = f grader,
          points      = IntInf.toInt points,
          description = description
        }
      | _ => raise Fail "Invalid rubric"
    in
      val fromJSON : (JSON.value -> 'a) -> JSON.value -> 'a t = fn f => JSONUtil.arrayMap (itemFromJSON f)
    end
  end
