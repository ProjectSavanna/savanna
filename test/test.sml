local
  structure IntScore :> SCORE where type t = int =
    struct
      type t = Int.int
      val z = 0
      val f = Int.+
      val toString = fn
        1 => "1 point"
      | n => Int.toString n ^ " points"
      val fromJSON = JSONUtil.asInt
    end
  structure A = Assignment (Problem (CodeGrader (IntScore)))
  val a = A.load (Filename.$ "demo") ()
  val [writeup] = CommandLine.arguments ()
in
  val () = A.writeup a "article" (Filename.$ writeup)
end

val () = OS.Process.exit OS.Process.success
