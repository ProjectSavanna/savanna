structure Library :> LIBRARY =
  struct
    type t = string

    val compare = String.compare

    val op / = OS.Path.concat
    val stage = fn path => fn library => fn location =>
      FileUtils.copyTree (path / library / "src", location)
  end
