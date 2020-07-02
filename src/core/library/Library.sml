structure Library :> LIBRARY =
  struct
    type t = string

    val compare = String.compare

    val op / = Filename.concat
    val stage = fn path => fn library => fn location => FileUtils.copyTree (
      path / Filename.` library / Filename.` "src",
      location
    )
  end
