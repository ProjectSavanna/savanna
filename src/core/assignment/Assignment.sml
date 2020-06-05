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

    val op ^ = OS.Path.concat
    val dateFromJSON = (fn SOME d => d | NONE => raise Fail "Invalid date format") o Date.fromString o JSONUtil.asString
    val load = fn path => Remote.hide {
      path = path ^ "assignment.json",
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
            problems = JSONUtil.arrayMap (Problem.load o Fn.curry (op ^) (path ^ "problems") o JSONUtil.asString) problems,
            dates = {
              out = dateFromJSON out,
              due = dateFromJSON due
            }
          } : t
        | _ => raise Fail "Invalid problem"
      )
    }

  end
