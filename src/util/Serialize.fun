functor Serialize (J : JSONABLE) :> SERIALIZABLE where type t = J.t =
  struct
    type t = J.t
    val load = J.fromJSON o JSONParser.parseFile
    val save = fn path => fn s => (
      let
        val file = TextIO.openOut path
      in
        JSONPrinter.print' {pretty=true,strm=file} (J.toJSON s) before TextIO.closeOut file
      end
    )
  end
