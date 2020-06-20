signature PROBLEM =
  sig
    include CONFIG where type t = {
      name      : string,
      root      : Filename.t,
      tasks     : Task.t Remote.t list,
      files     : Filename.t list,
      libraries : string list,
      grader    : {
        helpers : Filename.t list,
        style   : string list
      }
    }

    val stage : t * Filename.t -> unit
  end
