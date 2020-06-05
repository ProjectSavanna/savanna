structure Kind =
  struct
    datatype t = Hw | Lab

    val toString = fn
      Hw  => "hw"
    | Lab => "lab"

    val fromString = fn
      "hw"  => SOME Hw
    | "lab" => SOME Lab
    | _     => NONE

    val fromJSON = Option.valOf o fromString o JSONUtil.asString
  end