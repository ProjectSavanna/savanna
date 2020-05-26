structure Util =
  struct
    val sum = List.foldr (op +) 0
  end
