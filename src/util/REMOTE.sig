signature REMOTE =
  sig
    type 'a t

    val hide : {
      path : Filename.t,
      get  : Filename.t -> 'a
    } -> 'a t

    val !  : 'a t -> 'a
  end
