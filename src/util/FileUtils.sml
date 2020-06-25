structure FileUtils =
  struct

    structure FileSys = OS.FileSys
    structure Path = OS.Path

    exception RmError

    val rec rmTree = fn path => (
      case FileSys.isLink path of
        true  => raise RmError
      | false => (
          let
            val ds = FileSys.openDir path
            val rec loop = fn () => (
              case FileSys.readDir ds of
                NONE      => FileSys.closeDir ds
              | SOME name => (
                  let
                    val fullName = Path.concat (path,name)
                  in
                    (
                      case FileSys.isDir fullName of
                        false => FileSys.remove fullName
                      | true  => rmTree fullName
                    ); loop ()
                  end
                )
            )
          in
            loop (); FileSys.rmDir path
          end
        )
    )

    val rec mkDirs = fn path => (
      case FileSys.isDir path of
        false => FileSys.mkDir path
      | true  => ()
    ) handle OS.SysErr _ => (mkDirs (Path.getParent path); FileSys.mkDir path)

    val copyFile = fn (src,dst) => (
      let
        val () = mkDirs (Path.getParent dst)
        val srcStream = TextIO.openIn src
        val dstStream = TextIO.openOut dst
      in
        TextIO.output (dstStream,TextIO.inputAll srcStream);
        TextIO.closeIn srcStream;
        TextIO.closeOut dstStream
      end
    )

    val rec copyTree = fn (src,dst) => (
      let
        val ds = FileSys.openDir src
        val rec loop = fn () => (
          case FileSys.readDir ds of
            NONE      => FileSys.closeDir ds
          | SOME name => (
              let
                val srcName = Path.concat (src,name)
                val dstName = Path.concat (dst,name)
              in
                (
                  case FileSys.isDir srcName of
                    false => copyFile
                  | true  => copyTree
                ) (srcName,dstName); loop ()
              end
            )
        )
      in
        FileSys.mkDir dst;
        loop ()
      end
    )

    val read = fn filename => (
      let
        val stream = TextIO.openIn filename
      in
        TextIO.inputAll stream before TextIO.closeIn stream
      end
    )

    val foldl = fn g => fn start => fn stream => (
      let
        val rec loop = fn state => (
          case TextIO.inputLine stream of
            NONE      => state before TextIO.closeIn stream
          | SOME line => loop (g (line,state))
        )
      in
        loop start handle e => (TextIO.closeIn stream; raise e)
      end
    )

    val write = fn (openFn : string -> TextIO.outstream) => fn (filename,contents) => (
      let
        val stream = openFn filename
      in
        TextIO.output (stream,contents);
        TextIO.closeOut stream
      end
    )

  end
