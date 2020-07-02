structure Remote :> REMOTE =
  struct
    type 'a t = {
      path : Filename.t,
      get  : Filename.t -> 'a
    }

    val hide = fn {path=path,get=get} => {
      path = OS.Path.mkAbsolute {
        path = path,
        relativeTo = OS.FileSys.getDir ()
      },
      get  = get
    }

    val ! = fn {path,get} => get path
  end
