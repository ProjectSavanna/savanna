functor Problem (Score :
  sig
    include MONOID SHOW
    val fromJSON : JSON.value -> t
  end
) :> PROBLEM where Score = Score =
  struct
    structure Score = Score

    type t = {
      root      : Filename.absolute Filename.t,
      files     : Filename.relative Filename.t list,
      tasks     : {
        name   : string,
        points : Score.t
      } list,
      libraries : Library.t list
    }

    val op / = Filename.concat
    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        case FileUtils.parseJSON (path / Filename.` "problem.json") of
          JSON.OBJECT [
            ("files"    , files    ),
            ("tasks"    , tasks    ),
            ("libraries", libraries)
          ] => {
            root = path,
            files = JSONUtil.arrayMap (Filename.` o JSONUtil.asString) files,
            tasks = JSONUtil.arrayMap
              (fn obj => {
                name   = JSONUtil.asString (JSONUtil.lookupField obj "name"),
                points = Score.fromJSON (JSONUtil.lookupField obj "points")
              })
              tasks,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries
          }
        | _ => raise Fail ("Invalid problem at " ^ Filename.toString path)
      )
    }

    val score : t -> Score.t = List.foldr Score.f Score.z o List.map #points o #tasks

    local
      structure N = LaTeX.Number
      structure M = LaTeX.Macro
      val makeSwitch =
        List.foldri
          (fn (i,{name=name,points=score},acc) =>
            M.IfNum (
              (N.Counter "task",EQUAL,N.Constant (i + 1)),  (* check if task counter (one-indexed) matches *)
              M.NewLine (
                M.IfStrEqual (  (* cross-validate label in writeup with expected label *)
                  ("#1",name),
                  M.Text (Score.toString score),
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
          M.Def ("taskscore","#1",makeSwitch (#tasks problem)),
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

    val grader = fn problem : t => fn location => #libraries problem before
      FileUtils.copyTree (#root problem / Filename.` "grader", location)
  end
