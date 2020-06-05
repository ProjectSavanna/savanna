local
  signature DATATYPE =
    sig
      datatype t
        = Code of {
            name   : string,
            file   : Filename.t,
            rubric : Filename.t Rubric.t
          }
        | Written of {
            name   : string,
            points : int,
            rubric : unit Rubric.t
          }
    end
in
  signature TASK =
    sig
      structure Rubric : RUBRIC

      include CONFIG
      include DATATYPE

      val isCode    : t -> bool
      val isWritten : t -> bool
    end
end
