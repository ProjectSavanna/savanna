functor Serialize (S : SERIALIZABLE) =
  struct
    val load : Filename.t -> S.t = S.fromJSON o JSONParser.parseFile
    val save : Filename.t -> S.t -> unit = fn filename => fn s => (
      let
        val file = TextIO.openOut filename
      in
        JSONPrinter.print' {pretty=true,strm=file} (S.toJSON s) before TextIO.closeOut file
      end
    )
  end
