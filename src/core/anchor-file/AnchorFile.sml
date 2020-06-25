structure AnchorFile :> ANCHORFILE =
  struct
    type t = Filename.t * Filename.t
    val fromPath = fn (prefix,path) => Remote.hide {
      path = prefix,
      get = fn prefix => (
        prefix, path
      )
    }

    val op / = OS.Path.concat
    val stage = fn ((prefix,path),location) =>
      FileUtils.copyFile (prefix / path, location / path)
  end
