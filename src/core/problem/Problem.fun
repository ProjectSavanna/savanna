functor Problem (Score : SCORE) :> PROBLEM where Score = Score =
  struct
    structure Score = Score

    type t = {
      root      : Filename.absolute Filename.t,
      code      : bool,
      tasks     : {
        name   : string,
        points : Score.t,
        filename : Filename.relative Filename.t
      } list,
      libraries : LibrarySet.set
    }

    val op / = Filename.concat
    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        case FileUtils.parseJSON (path / Filename.` "problem.json") of
          JSON.OBJECT [
            ("code"     , code     ),
            ("tasks"    , tasks    ),
            ("libraries", libraries)
          ] => {
            root = path,
            code = JSONUtil.asBool code,
            tasks = JSONUtil.arrayMap
              (fn obj => {
                name   = JSONUtil.asString (JSONUtil.lookupField obj "name"),
                points = Score.fromJSON (JSONUtil.lookupField obj "points"),
                filename = Filename.` (JSONUtil.asString (JSONUtil.lookupField obj "filename"))
              })
              tasks,
            libraries = (
              List.foldr
                (Fn.flip (Fn.uncurry LibrarySet.insert))
                LibrarySet.empty
                (JSONUtil.arrayMap JSONUtil.asString libraries)
            )
          }
        | _ => raise Fail ("Invalid problem at " ^ Filename.toString path)
      )
    }

    val score : t -> Score.t = List.foldr Score.f Score.z o List.map #points o #tasks

    local
      structure N = LaTeX.Number
      structure M = LaTeX.Macro
      val makeSwitch = fn codepath =>
        List.foldri
          (fn (i,{name=name,points=score,filename=filename},acc) =>
            M.IfNum (
              (N.Counter "task",EQUAL,N.Constant (i + 1)),  (* check if task counter (one-indexed) matches *)
              M.NewLine (
                M.IfStrEqual (  (* cross-validate label in writeup with expected label *)
                  ("#1",name),
                  M.Concat (
                    M.Text (Score.toString score ^ ", "),
                    M.Font (LaTeX.Font.TeleType, M.Text (Filename.toString (codepath / filename)))
                  ),
                  M.Error (M.Concat (M.Text ("Invalid placement of task: " ^ name ^ " at "), M.GetCounter "task"))
                )
              ),
              acc
            )
          )
          (M.Error (M.Text "Writeup contains more tasks than were expected"))
    in
      val writeup = fn problem : t => fn codepath => M.NewLine (
        List.foldMapr M.Concat M.NewLine (M.Text "") [
          M.Def ("codepath","",M.Text (Filename.toString codepath ^ "/")),  (* set \codepath, used by \path{} *)
          M.Def ("taskscore","#1",makeSwitch codepath (#tasks problem)),
          M.Import (#root problem / Filename.` "writeup","writeup"),
          M.ClearPage,
          M.StepCounter "problem",
          M.IfNum (  (* guarantee each problem includes all tasks *)
            (N.Counter "task",EQUAL,N.Constant (List.length (#tasks problem))),
            M.Text "",
            M.Error (M.Text ("Incorrect number of tasks in problem: " ^ Filename.toString (#root problem)))
          ),
          M.IfNum (  (* guarantee each problem uses exactly one section, so taskscore switch works *)
            (N.Counter "problem",EQUAL,N.Counter "section"),
            M.Text "",
            M.Error (M.Text ("Problem used multiple sections: " ^ Filename.toString (#root problem)))
          )
        ]
      )
    end

    val handout = fn problem : t => fn location => #libraries problem before (
      case #code problem of
        false => ()
      | true  => FileUtils.copyTree (#root problem / Filename.` "code", location)
    )

    val grader = fn problem : t => fn location => #libraries problem before
      FileUtils.copyTree (#root problem / Filename.` "grader", location)
  end
