structure Assignment :> ASSIGNMENT =
  struct
    structure Kind = Kind

    type t = {
      name     : string,
      title    : string,
      kind     : Kind.t,
      problems : Problem.t Remote.t list,
      dates    : {
        out : Date.date,
        due : Date.date
      }
    }

    val getName : t -> string = #name
    val getPoints : t -> int = Util.sum o List.map (Problem.getPoints o Remote.!) o #problems

    val op / = OS.Path.concat
    val dateFromJSON = (fn SOME d => d | NONE => raise Fail "Invalid date format") o Date.fromString o JSONUtil.asString
    val load = fn path => Remote.hide {
      path = path / "assignment.json",
      get = fn filename => (
        case JSONParser.parseFile filename of
          JSON.OBJECT [
            ("name", name),
            ("title", title),
            ("kind", kind),
            ("problems", problems),
            ("dates", JSON.OBJECT [
              ("out", out),
              ("due", due)
            ])
          ] => {
            name = JSONUtil.asString name,
            title = JSONUtil.asString title,
            kind = Kind.fromJSON kind,
            problems = JSONUtil.arrayMap (Problem.load o Fn.curry (op /) (path / "problems") o JSONUtil.asString) problems,
            dates = {
              out = dateFromJSON out,
              due = dateFromJSON due
            }
          } : t
        | _ => raise Fail "Invalid problem"
      )
    }

    local
      infix |>
      val op |> = Util.|>
      val stageCode = fn (problems : Problem.t list, location) => (
        OS.FileSys.mkDir location;
        List.app (fn problem =>
          case List.null (#files problem) of
            false => Problem.stage (problem, location / #name problem)
          | true  => ()
        ) problems
      )
      val stageLibraries = fn (problems : Problem.t list, location) => (
        OS.FileSys.mkDir location;
        problems
        |> List.concatMap #libraries
        |> Util.unique Library.compare
        |> List.app (fn library =>
            Library.stage (library, location / #name library)
          )
      )
    in
      val stage = fn (assignment : t, location) => (
        let
          val problems =
            assignment
            |> #problems
            |> List.map Remote.!
        in
          OS.FileSys.mkDir location;
          stageCode (problems, location / "code");
          stageLibraries (problems, location / "lib")
        end
      )
    end
  end
