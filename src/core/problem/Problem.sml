functor Problem (Grader : GRADER) :> PROBLEM =
  struct
    type t = {
      root      : Filename.t,
      grader    : Grader.t Remote.t,
      files     : Filename.t list,
      libraries : Library.t list
    }

    val op / = OS.Path.concat
    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        case JSONParser.parseFile (path / "problem.json") of
          JSON.OBJECT [
            ("files"    , files    ),
            ("libraries", libraries)
          ] => {
            root = path,
            grader = Grader.load (path / "grader"),
            files = JSONUtil.arrayMap JSONUtil.asString files,
            libraries = JSONUtil.arrayMap JSONUtil.asString libraries
          }
        | _ => raise Fail ("Invalid problem at " ^ path)
      )
    }

    local
      val concatWith = fn sep => String.concat o List.map (Fn.curry (Fn.flip (op ^)) sep)
      val makeSwitch =
        concatWith "%\n"
        o List.foldri
            (fn (i,(name,score),acc) =>
              "\\ifnum \\value{task} = " ^ Int.toString (i + 1) ^ " "
              :: "\\ifstrequal{#1}{" ^ name ^ "}{" ^ Grader.Score.toString score ^ "}{\\GenericError{}{Invalid placement of task: " ^ name ^ " at \\thetask}{}{}}"
              :: "\\else"
              :: acc @ ["\\fi"]
            )
            ["\\GenericError{}{Too many tasks!}{}{}"]
        o Grader.tasks
    in
      val writeup = fn problem : t => fn codepath => concatWith "\n" [
        "\\def\\codepath{" ^ codepath ^ "}",
        "\\def\\attribute#1{" ^ makeSwitch (Remote.! (#grader problem)) ^ "}",
        "\\import{" ^ #root problem ^ "/writeup/}{writeup}",
        "\\clearpage",
        "\\stepcounter{problem}",
        "\\ifnum \\value{problem} = \\value{section} \\else \\GenericError{}{Oh no! Multiple sections in problem from " ^ #root problem ^ ".}{}{} \\fi"
      ]
    end

    val handout = fn problem : t => fn location => #libraries problem before (
      case List.null (#files problem) of
        false => FileUtils.copyTree (#root problem / "code", location)
      | true  => ()
    )

    val grader = fn problem : t => fn location =>
      Grader.stage (Remote.! (#grader problem)) location @ #libraries problem
  end
