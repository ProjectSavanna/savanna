signature ASSIGNMENT =
  sig
    include CONFIG where type t = {
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
