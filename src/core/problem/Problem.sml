structure Problem :> PROBLEM =
  struct
    type t = {
      name      : string,
      root      : Filename.t,
      files     : Filename.t list,
      libraries : Library.t list
    }

    val getName : t -> string = #name

    val op / = OS.Path.concat
    val load = fn path => Remote.hide {
      path = path / "problem.json",
      get = fn filename => (
        case JSONParser.parseFile filename of
          JSON.OBJECT [
            ("name"     , name     ),
            ("files"    , files    ),
            ("libraries", libraries)
          ] => {
            name = JSONUtil.asString name,
            root = path,
            files = JSONUtil.arrayMap JSONUtil.asString files,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries
          }
        | _ => raise Fail ("Invalid problem at " ^ path)
      )
    }

    local
      infix |>
      val op |> = Util.|>
      val stageCode = fn (problems : t list, location) => (
        OS.FileSys.mkDir location;
        List.app (fn problem =>
          case List.null (#files problem) of
            false => FileUtils.copyTree (#root problem / "code", location / #name problem)
          | true  => ()
        ) problems
      )
      val stageLibraries = fn stage => fn (problems : t list, location) => (
        OS.FileSys.mkDir location;
        problems
        |> List.concatMap #libraries
        |> Util.unique Library.compare
        |> List.app (fn library => stage library (location / library))
      )
    in
      val handout = fn problems => fn stageLibrary => fn location => (
        OS.FileSys.mkDir location;
        stageCode (problems, location / "code");
        stageLibraries stageLibrary (problems, location / "lib")
      )
    end
  end
