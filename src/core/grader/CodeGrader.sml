structure CodeGrader :> GRADER =
  struct
    structure Score =
      struct
        type t = Int.int
        val z = 0
        val f = Int.+
        val toString = fn
          1 => "1 point"
        | n => Int.toString n ^ " points"

        val fromJSON = JSONUtil.asInt
      end

    type t = Filename.absolute Filename.t * {
      name   : string,
      points : Score.t
    } list

    val op / = Filename.concat

    val load = fn path => Remote.hide {
      path = path,
      get = fn path => (
        path,
        JSONUtil.arrayMap
          (fn obj => {
            name   = JSONUtil.asString (JSONUtil.lookupField obj "name"),
            points = Score.fromJSON (JSONUtil.lookupField obj "points")
          })
          (JSONParser.parseFile (Filename.toString (path / Filename.` "grader.json")))
      )
    }

    val stage = fn (root,_) => fn location =>
      nil before FileUtils.copyTree (root,location)

    val tasks = fn (_,tasks) : t => List.map (fn item => (#name item, #points item)) tasks
  end
