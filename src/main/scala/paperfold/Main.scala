package paperfold

object Main:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("usage: paperfold <input-file>")
      sys.exit(1)
    val (rows, cols, instructions) = Input.readPath(args.head)
    val paper = Paper.create(rows, cols)
    val folded = Folder.applyAll(paper, instructions)
    println(Paper.show(folded))
