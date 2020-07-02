structure Remote :> REMOTE =
  struct
    type 'a t = {
      path : Filename.absolute Filename.t,
      get  : Filename.absolute Filename.t -> 'a
    }

    val hide = Fn.id

    val ! = fn {path,get} => get path
  end
