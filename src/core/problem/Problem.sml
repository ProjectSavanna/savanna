functor Problem (Grader : GRADER) :> PROBLEM =
  struct
    type t = {
      root      : Filename.t,
      grader    : Grader.t Remote.t,
      files     : Filename.t list,
      libraries : Library.t list
    }

    val op / = OS.Path.concat
    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        case JSONParser.parseFile (path / "problem.json") of
          JSON.OBJECT [
            ("files"    , files    ),
            ("libraries", libraries)
          ] => {
            root = path,
            grader = Grader.load (path / "grader"),
            files = JSONUtil.arrayMap JSONUtil.asString files,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries
          }
        | _ => raise Fail ("Invalid problem at " ^ path)
      )
    }

    val handout = fn problem : t => fn location => #libraries problem before (
      case List.null (#files problem) of
        false => FileUtils.copyTree (#root problem / "code", location)
      | true  => ()
    )

    val grader = fn problem : t => fn location =>
      Grader.stage (Remote.! (#grader problem)) location @ #libraries problem
  end
