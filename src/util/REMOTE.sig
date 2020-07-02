signature REMOTE =
  sig
    type 'a t

    val hide : {
      path : Filename.absolute Filename.t,
      get  : Filename.absolute Filename.t -> 'a
    } -> 'a t

    val !  : 'a t -> 'a
  end
