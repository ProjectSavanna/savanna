structure CodeGrader :> GRADER =
  struct
    type t = Filename.t

    val load = fn path => Remote.hide {
      path = path,
      get = Fn.id
    }

    val stage = fn root => fn location =>
      nil before FileUtils.copyTree (root,location)
  end
