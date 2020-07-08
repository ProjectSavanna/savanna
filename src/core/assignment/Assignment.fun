functor Assignment (Problem : PROBLEM) :> ASSIGNMENT =
  struct
    type t = {
      name     : string,
      title    : string,
      problems : (string * Problem.t Remote.t) list,
      dates    : {
        out : Date.date,
        due : Date.date
      }
    }

    val op / = Filename.concat

    val CODE = Filename.` "code"

    local
      val dateFromJSON = (fn SOME d => d | NONE => raise Fail "Invalid date format") o Date.fromString o JSONUtil.asString
      val loadProblem = fn path => fn name => (
        name,
        Problem.load (path / Filename.` "problems" / Filename.` name)
      )
    in
      val load = fn path => fn () => (
        case FileUtils.parseJSON (path / Filename.` "assignment.json") of
          JSON.OBJECT [
            ("name", name),
            ("title", title),
            ("problems", problems),
            ("dates", JSON.OBJECT [
              ("out", out),
              ("due", due)
            ])
          ] => {
            name = JSONUtil.asString name,
            title = JSONUtil.asString title,
            problems = JSONUtil.arrayMap (loadProblem path o JSONUtil.asString) problems,
            dates = {
              out = dateFromJSON out,
              due = dateFromJSON due
            }
          }
        | _ => raise Fail "Invalid problem"
      )
    end

    local
      structure M = LaTeX.Macro
    in
      val writeup = fn assignment : t => fn documentclass => fn location => (
        FileUtils.write TextIO.openOut (
          Filename.toString location,
          LaTeX.Macro.toString (
            List.foldMapr M.Concat M.NewLine (M.Text "") [
              M.DocumentClass documentclass,
              M.NewCounter "problem",
              M.UsePackage (SOME "subpreambles=true,sort=true,mode=buildnew","standalone"),
              M.UsePackage (NONE,"import"),
              M.UsePackage (NONE,"etoolbox"),
              M.Title (#title assignment),
              M.Environment (
                "document",
                M.Concat (
                  M.Concat (M.TableOfContents,M.ClearPage),
                  List.foldMapr
                    M.Concat
                    (fn (name,problem) =>
                      Problem.writeup
                        (problem ())
                        (CODE / Filename.` name)
                    )
                    (M.Text "")
                    (#problems assignment)
                )
              )
            ]
          )
        )
      )
    end

    local
      infix |>
      val op |> = Util.|>
      val stageCode = fn stage => fn location => fn problems => (
        OS.FileSys.mkDir (Filename.toString location);
        problems
        |> List.map (fn (name,problem) => stage problem (location / Filename.` name))
        |> List.foldr (Fn.uncurry LibrarySet.union) LibrarySet.empty
      )
      val LIBRARIES = (
        case OS.Process.getEnv "SAVANNA_LIBRARIES" of
          NONE   => raise Fail "Environment variable SAVANNA_LIBRARIES not set"
        | SOME d => (
            case Filename.absolute d of
              NONE   => raise Fail "Environment variable SAVANNA_LIBRARIES not an absolute path"
            | SOME p => p
          )
      )
      val stageLibrary = fn location => fn library =>
        Library.stage LIBRARIES library (location / Filename.` library)
      val stageLibraries = fn location => fn libraries => (
        OS.FileSys.mkDir (Filename.toString location);
        libraries
        |> LibrarySet.app (stageLibrary location)
      )
      val aux = fn (stage,dir) => fn assignment : t => fn location => (
        OS.FileSys.mkDir (Filename.toString location);
        assignment
        |> #problems
        |> List.map (fn (name,r) => (name, r ()))
        |> stageCode stage (location / dir)
        |> stageLibraries (location / Filename.` "lib")
      )
    in
      val handout = aux (Problem.handout,CODE)
      val grader  = aux (Problem.grader,Filename.` "graders")
    end
  end
