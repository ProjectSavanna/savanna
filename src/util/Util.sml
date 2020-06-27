structure Util =
  struct
    infix |>
    fun x |> f = f x

    val sum = List.foldr (op +) 0

    val unique : ('a * 'a -> order) -> 'a list -> 'a list = fn cmp => (
      let
        val op > = Fn.curry (op =) GREATER o cmp
        val different = fn (x,y) => if cmp (x,y) = EQUAL then NONE else SOME y
        val zipWithTail = fn
          nil     => nil
        | x :: xs => x :: ListPair.mapPartial different (x::xs,xs)
      in
        zipWithTail o ListMergeSort.sort (op >)
      end
    )
  end
