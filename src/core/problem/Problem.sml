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

    val handout = fn problem : t => fn location => (
      OS.FileSys.mkDir location; (
        case List.null (#files problem) of
          false => FileUtils.copyTree (#root problem / "code", location / #name problem)
        | true  => ()
      ); #libraries problem
    )
  end
