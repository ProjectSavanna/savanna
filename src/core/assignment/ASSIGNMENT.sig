signature ASSIGNMENT =
  sig
    structure Kind : KIND

    include sig
      include CONFIG WRITABLE
    end where type t = {
      name     : string,
      title    : string,
      kind     : Kind.t,
      problems : Problem.t Remote.t list,
      dates    : {
        out : Date.date,
        due : Date.date
      }
    }
  end
