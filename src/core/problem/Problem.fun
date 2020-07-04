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
      datatype number = datatype LaTeX.number
      datatype latex = datatype LaTeX.t
      val makeSwitch =
        List.foldri
          (fn (i,(name,score),acc) =>
            IfNum (
              (Counter "task",EQUAL,Constant (i + 1)),  (* check if task counter (one-indexed) matches *)
              NewLine (
                IfStrEqual (  (* cross-validate label in writeup with expected label *)
                  ("#1",name),
                  Text (Grader.Score.toString score),
                  Error (Concat (Text ("Invalid placement of task: " ^ name ^ " at "), GetCounter "task"))
                )
              ),
              acc
            )
          )
          (Error (Text "Writeup contains more tasks than were expected"))
        o Grader.tasks
    in
      val writeup = fn problem : t => fn codepath => NewLine (
        List.foldMapr Concat NewLine (Text "") [
          Def ("codepath","",Text (Filename.toString codepath ^ "/")),  (* set \codepath, used by \path{} *)
          Def ("taskscore","#1",makeSwitch (Remote.! (#grader problem))),
          Import (#root problem / Filename.` "writeup","writeup"),
          ClearPage,
          StepCounter "problem",
          IfNum (  (* guarantee each problem uses exactly one section, so taskscore switch works *)
            (Counter "problem",EQUAL,Counter "section"),
            Text "",
            Error (Text ("Problem used multiple sections: " ^ Filename.toString (#root problem)))
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
