package paperfold

object Main:
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      System.err.println("usage: paperfold <input-file>")
      sys.exit(1)
    Input.readPath(args.head) match
      case Job.Fold(length, width, instructions) =>
        val folded = Folder.applyAll(Paper.create(length, width), instructions)
        println(Paper.show(folded))
      case Job.Unfold(folded) =>
        println(Unfolder.show(Unfolder.instructionsFor(folded)))
