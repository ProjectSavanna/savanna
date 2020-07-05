structure LaTeX =
  struct
    type counter = string

    structure Number =
      struct
        datatype t
          = Constant of int
          | Counter of counter

        val toString = fn
          Constant n => Int.toString n
        | Counter c  => "\\value{" ^ c ^ "}"
      end

    structure Macro =
      struct
        datatype t
          = Text of string
          | IfNum of (Number.t * order * Number.t) * t * t
          | IfStrEqual of (string * string) * t * t
          | Error of t
          | Def of string * string * t
          | ClearPage
          | TableOfContents
          | GetCounter of counter
          | StepCounter of counter
          | NewCounter of counter
          | Import of Filename.absolute Filename.t * string
          | UsePackage of string option * string
          | Environment of string * t
          | DocumentClass of string
          | Concat of t * t
          | NewLine of t

        val rec toString = fn
          Text s => s
        | IfNum ((x,r,y),t,e) => (
            "\\ifnum " ^ Number.toString x ^ " " ^ (
              case r of
                LESS    => "<"
              | EQUAL   => "="
              | GREATER => ">"
            ) ^ " " ^ Number.toString y ^ " " ^ toString t ^ "\\else" ^ toString e ^ "\\fi"
          )
        | IfStrEqual ((x,y),t,e) => "\\ifstrequal{" ^ x ^ "}{" ^ y ^ "}{" ^ toString t ^ "}{" ^ toString e ^ "}"
        | Error message => "\\GenericError{}{" ^ toString message ^ "}{}{}"
        | Def (name,args,res) => "\\def\\" ^ name ^ args ^ "{" ^ toString res ^ "}"
        | ClearPage => "\\clearpage"
        | TableOfContents => "\\tableofcontents"
        | GetCounter counter => "\\the" ^ counter
        | StepCounter counter => "\\stepcounter{" ^ counter ^ "}"
        | NewCounter counter => "\\newcounter{" ^ counter ^ "}"
        | Import (path,file) => "\\import{" ^ Filename.toString path ^ "/}{" ^ file ^ "}"
        | UsePackage (options,package) => (
            "\\usepackage" ^ (
              case options of
                NONE      => ""
              | SOME opts => "[" ^ opts ^ "]"
            )
            ^ "{" ^ package ^ "}"
          )
        | Environment (name,a) => "\\begin{" ^ name ^ "}" ^ toString a ^ "\\end{" ^ name ^ "}"
        | DocumentClass class => "\\documentclass{" ^ class ^ "}"
        | Concat (a,b) => toString a ^ toString b
        | NewLine a => toString a ^ "%\n"
      end
  end