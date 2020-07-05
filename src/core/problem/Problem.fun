functor Problem (Grader : GRADER) :> PROBLEM =
  struct
    type t = {
      root      : Filename.absolute Filename.t,
      grader    : Grader.t Remote.t,
      files     : Filename.relative Filename.t list,
      libraries : Library.t list
    }

    val op / = Filename.concat
    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        case FileUtils.parseJSON (path / Filename.` "problem.json") of
          JSON.OBJECT [
            ("files"    , files    ),
            ("libraries", libraries)
          ] => {
            root = path,
            grader = Grader.load (path / Filename.` "grader"),
            files = JSONUtil.arrayMap (Filename.` o JSONUtil.asString) files,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries
          }
        | _ => raise Fail ("Invalid problem at " ^ Filename.toString path)
      )
    }

    local
      structure N = LaTeX.Number
      structure M = LaTeX.Macro
      val makeSwitch =
        List.foldri
          (fn (i,(name,score),acc) =>
            M.IfNum (
              (N.Counter "task",EQUAL,N.Constant (i + 1)),  (* check if task counter (one-indexed) matches *)
              M.NewLine (
                M.IfStrEqual (  (* cross-validate label in writeup with expected label *)
                  ("#1",name),
                  M.Text (Grader.Score.toString score),
                  M.Error (M.Concat (M.Text ("Invalid placement of task: " ^ name ^ " at "), M.GetCounter "task"))
                )
              ),
              acc
            )
          )
          (M.Error (M.Text "Writeup contains more tasks than were expected"))
        o Grader.tasks
    in
      val writeup = fn problem : t => fn codepath => M.NewLine (
        List.foldMapr M.Concat M.NewLine (M.Text "") [
          M.Def ("codepath","",M.Text (Filename.toString codepath ^ "/")),  (* set \codepath, used by \path{} *)
          M.Def ("taskscore","#1",makeSwitch (Remote.! (#grader problem))),
          M.Import (#root problem / Filename.` "writeup","writeup"),
          M.ClearPage,
          M.StepCounter "problem",
          M.IfNum (  (* guarantee each problem uses exactly one section, so taskscore switch works *)
            (N.Counter "problem",EQUAL,N.Counter "section"),
            M.Text "",
            M.Error (M.Text ("Problem used multiple sections: " ^ Filename.toString (#root problem)))
          )
        ]
      )
    end

    val handout = fn problem : t => fn location => #libraries problem before (
      case List.null (#files problem) of
        false => FileUtils.copyTree (#root problem / Filename.` "code", location)
      | true  => ()
    )

    val grader = fn problem : t => fn location =>
      Grader.stage (Remote.! (#grader problem)) location @ #libraries problem
  end
