structure AnchorFile :> ANCHORFILE =
  struct
    structure Parse =
      struct
        type anchor = {
          task     : string,
          solution : string list,
          handout  : string list
        }
        datatype part = Solution | Handout
        datatype mode = Free | MidAnchor of part * anchor
        datatype block
          = Anchor of anchor
          | Line of string

        val insertLine = fn (line : string, (mode : mode, result : block list)) => (
          case line of
            "begin\n" => (
              case mode of
                Free => (
                  MidAnchor (
                    Solution,
                    {
                      task     = "TODO: TASK NAME",
                      solution = nil,
                      handout  = nil
                    }
                  ),
                  result
                )
              | _ => raise Fail "Invalid anchor begin"
            )
          | "handout\n" => (
              case mode of
                MidAnchor (Solution,anchor) => (MidAnchor (Handout,anchor), result)
              | _ => raise Fail "Invalid anchor handout"
            )
          | "end\n" => (
              case mode of
                MidAnchor (Handout,anchor) => (Free, Anchor anchor :: result)
              | _ => raise Fail "Invalid anchor end"
            )
          | _ => (
              case mode of
                Free => (Free, Line line :: result)
              | MidAnchor (Solution, {task,solution,handout}) => (
                  MidAnchor (Solution, {task=task, solution=line::solution, handout=handout}),
                  result
                )
              | MidAnchor (Handout, {task,solution,handout}) => (
                  MidAnchor (Handout, {task=task, solution=solution, handout=line::handout}),
                  result
                )
            )
        )

        val reverse = fn
          Anchor {task,solution,handout} => Anchor {
            task     = task,
            solution = List.rev solution,
            handout  = List.rev handout
          }
        | Line line => Line line

        val parse =
          List.rev
          o List.map reverse
          o #2
          o FileUtils.foldl insertLine (Free,nil)
          o TextIO.openIn
      end

    type t = Filename.t * Parse.block list

    val op / = OS.Path.concat

    val fromPath = fn (prefix,path) => Remote.hide {
      path = prefix,
      get = fn prefix => (path, Parse.parse (prefix / path))
    }

    val stage = fn ((path,blocks),location) => (
      let
        val stream = TextIO.openOut (location / path)
        val writeBlock = fn
          Parse.Anchor {handout,...} => List.app (Fn.curry TextIO.output stream) handout
        | Parse.Line line => TextIO.output (stream,line)
      in
        List.app writeBlock blocks before TextIO.closeOut stream
      end
    )
  end
