functor CodeGrader (Score : SCORE) :> GRADER where Score = Score =
  struct
    structure Score = Score

    type leaf = Filename.absolute Filename.t * Score.t
    datatype t
      = Leaf of leaf
      | Node of t list

    val op / = Filename.concat
    val load = fn path => fn () => Leaf (
      let
        val json = FileUtils.parseJSON (path / Filename.` "task.json")
      in
        (path,Score.fromJSON (JSONUtil.lookupField json "score"))
      end
    )

    val rec score = fn
      Leaf (_,s) => s
    | Node gs    => List.foldr Score.f Score.z (List.map score gs)
    val combine = Node

    val rec build = fn grader => fn path => (
      case grader of
        Leaf (loc,_) => LibrarySet.empty before FileUtils.copyTree (loc,path)
      | Node gs      => (
          OS.FileSys.mkDir (Filename.toString path);
          List.foldri
            (fn (k,g,libs) => LibrarySet.union (build g (path / Filename.` (Int.toString k))) libs)
            LibrarySet.empty
            gs
        )
    )
  end
