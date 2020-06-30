local
  type handout = (Library.t -> Filename.t -> unit) -> Filename.t -> unit
in
  signature PROBLEM =
    sig
      include CONFIG
      val handout : t list -> handout
    end
end
