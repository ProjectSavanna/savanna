structure Problem :> PROBLEM =
  struct
    type t = {
      name      : string,
      root      : Filename.t,
      tasks     : Task.t Remote.t list,
      files     : Filename.t list,
      libraries : string list,
      grader    : {
        helpers : Filename.t list,
        style   : string list
      }
    }

    val getName : t -> string = #name
    val getPoints : t -> int = Util.sum o List.map (Task.getPoints o Remote.!) o #tasks

    val op ^ = OS.Path.concat
    val load = fn path => Remote.hide {
      path = path ^ "problem.json",
      get = fn filename => (
        case JSONParser.parseFile filename of
          JSON.OBJECT [
            ("name"     , name     ),
            ("tasks"    , tasks    ),
            ("files"    , files    ),
            ("libraries", libraries),
            ("grader"   , JSON.OBJECT [
              ("helpers", helpers),
              ("style"  , style  )
            ])
          ] => {
            name = JSONUtil.asString name,
            root = path,
            tasks = JSONUtil.arrayMap (Task.load o Fn.curry (op ^) (path ^ "tasks") o JSONUtil.asString) tasks,
            files = JSONUtil.arrayMap JSONUtil.asString files,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries,
            grader = {
              helpers = JSONUtil.arrayMap JSONUtil.asString helpers,
              style = JSONUtil.arrayMap JSONUtil.asString style
            }
          }
        | _ => raise Fail "Invalid problem"
      )
    }

    val stage = fn (problem : t, location) =>
      FileUtils.copyTree (#root problem ^ "code", location)
  end
