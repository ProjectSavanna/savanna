structure Library :> LIBRARY =
  struct
    type t = {
      name : string,
      root : Filename.t
    }

    val op ^ = OS.Path.concat

    val compare = fn (l1 : t, l2 : t) => String.compare (#name l1, #name l2)

    val fromName = fn name => (
      case OS.Process.getEnv "SAVANNA_LIBRARIES" of
        NONE      => raise Fail "Undefined environment variable: SAVANNA_LIBRARIES"
      | SOME path => {
          name = name,
          root = path ^ name
        }
    )

    val stage = fn (library : t, location) =>
      FileUtils.copyTree (#root library ^ "src", location)
  end
