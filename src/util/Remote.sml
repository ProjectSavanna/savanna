structure Remote :> REMOTE =
  struct
    type 'a t = {
      path : Filename.t,
      get  : Filename.t -> 'a
    }

    val hide = Fn.id

    val ! = fn {path,get} => get path
  end
