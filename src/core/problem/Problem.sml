functor Problem (Grader : GRADER) :> PROBLEM =
  struct
    type t = {
      name      : string,
      root      : Filename.t,
      grader    : Grader.t Remote.t,
      files     : Filename.t list,
      libraries : Library.t list
    }

    val getName : t -> string = #name

    val op / = OS.Path.concat
    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        case JSONParser.parseFile (path / "problem.json") of
          JSON.OBJECT [
            ("name"     , name     ),
            ("files"    , files    ),
            ("libraries", libraries)
          ] => {
            name = JSONUtil.asString name,
            root = path,
            grader = Grader.load path,
            files = JSONUtil.arrayMap JSONUtil.asString files,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries
          }
        | _ => raise Fail ("Invalid problem at " ^ path)
      )
    }

    val handout = fn problem : t => fn location => (
      OS.FileSys.mkDir location; (
        case List.null (#files problem) of
          false => FileUtils.copyTree (#root problem / "code", location / #name problem)
        | true  => ()
      ); #libraries problem
    )

    local
      val getGrader = fn problem : t => #grader problem
    in
      val grader = Grader.stage o Remote.! o getGrader
    end
  end
