structure Remote :> REMOTE =
  struct
    datatype 'a t = Remote of {
      path : Filename.t,
      get : Filename.t -> 'a
    }

    val ! = fn Remote {path,get,...} => get path
  end
