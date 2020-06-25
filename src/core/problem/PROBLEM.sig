signature PROBLEM =
  sig
    include sig
      include CONFIG WRITABLE
    end where type t = {
      name      : string,
      root      : Filename.t,
      tasks     : Task.t Remote.t list,
      files     : AnchorFile.t Remote.t list,
      libraries : Library.t list,
      grader    : {
        helpers : Filename.t list,
        style   : string list
      }
    }
  end
