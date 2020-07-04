structure LaTeX =
  struct
    type counter = string

    datatype number
      = Constant of int
      | Counter of counter

    datatype t
      = Text of string
      | IfNum of (number * order * number) * t * t
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

    local
      val numToString = fn
        Constant n => Int.toString n
      | Counter c  => "\\value{" ^ c ^ "}"
    in
      val rec toString = fn
        Text s => s
      | IfNum ((x,r,y),t,e) => (
          "\\ifnum " ^ numToString x ^ " " ^ (
            case r of
              LESS    => "<"
            | EQUAL   => "="
            | GREATER => ">"
          ) ^ " " ^ numToString y ^ " " ^ toString t ^ "\\else" ^ toString e ^ "\\fi"
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
