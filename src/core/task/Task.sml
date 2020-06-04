structure Task :> TASK =
  struct
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

    val getName = fn
      Code    data => #name data
    | Written data => #name data

    val getPoints = fn
      Code    data => (Util.sum o List.map #points o #rubric) data
    | Written data => #points data

    local
      structure S = Serialize (
        type t = t

        val toJSON   = fn
          Code {name,file,rubric} => JSON.OBJECT [
            ("name"  , JSON.STRING name                ),
            ("kind"  , JSON.STRING "code"              ),
            ("file"  , JSON.STRING file                ),
            ("rubric", Rubric.toJSON JSON.STRING rubric)
          ]
        | Written {name,points,rubric} => JSON.OBJECT [
            ("name"  , JSON.STRING name                         ),
            ("kind"  , JSON.STRING "written"                    ),
            ("points", JSON.INT (IntInf.fromInt points)         ),
            ("rubric", Rubric.toJSON (Fn.const JSON.NULL) rubric)
          ]
        val fromJSON = fn
          JSON.OBJECT [
            ("name"  , name              ),
            ("kind"  , JSON.STRING "code"),
            ("file"  , file              ),
            ("rubric", rubric            )
          ] => Code {
            name = JSONUtil.asString name,
            file = JSONUtil.asString file,
            rubric = Rubric.fromJSON JSONUtil.asString rubric
          }
        | JSON.OBJECT [
            ("name"  , name                 ),
            ("kind"  , JSON.STRING "written"),
            ("points", points               ),
            ("rubric", rubric               )
          ] => Written {
              name   = JSONUtil.asString name,
              points = JSONUtil.asInt points,
              rubric = Rubric.fromJSON (Fn.const ()) rubric
            }
        | _ => raise Fail "Invalid task"
      )
    in
      val load = fn path => S.load (path ^ "/task.json")
      val save = fn path => S.save (path ^ "/task.json")
    end

    val isCode = fn
      Code    _ => true
    | Written _ => false

    val isWritten = fn
      Code    _ => false
    | Written _ => true
  end
