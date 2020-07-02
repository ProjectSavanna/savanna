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
      | GetCounter of counter
      | StepCounter of counter
      | Import of Filename.t * string
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
      | GetCounter counter => "\\the" ^ counter
      | StepCounter counter => "\\stepcounter{" ^ counter ^ "}"
      | Import (path,file) => "\\import{" ^ path ^ "}{" ^ file ^ "}"
      | Concat (a,b) => toString a ^ toString b
      | NewLine a => toString a ^ "%\n"
    end
  end
