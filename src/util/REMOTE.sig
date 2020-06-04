signature REMOTE =
  sig
    datatype 'a t = Remote of {
      path : Filename.t,
      get : Filename.t -> 'a
    }

    val !  : 'a t -> 'a
  end
