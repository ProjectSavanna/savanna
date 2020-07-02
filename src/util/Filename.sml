structure Filename :> FILENAME =
  struct
    type 'a t = string

    type relative = unit  (* phantom *)
    val relative = fn path => (
      case OS.Path.isRelative path of
        false => NONE
      | true  => SOME path
    )
    val ` = Option.valOf o relative

    val concat = OS.Path.concat

    type absolute = unit  (* phantom *)
    val absolute = fn path => (
      case OS.Path.isAbsolute path of
        false => NONE
      | true  => SOME path
    )
    val getDir = OS.FileSys.getDir
    val $ = fn path => OS.Path.mkAbsolute {
      path       = path,
      relativeTo = getDir ()
    }

    val toString = Fn.id
  end
