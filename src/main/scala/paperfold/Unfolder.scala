package paperfold

/**
 * Recovers a shortest crease list that folds a blank labelled sheet into `target`.
 *
 * Original size is inferred from the stack length (`rows * cols == target.length`).
 * Among shortest lists, a more square sheet is preferred (so `CABD` yields `T 1`, `L 1`).
 */
object Unfolder:
  def instructionsFor(target: String): List[Instruction] =
    require(target.nonEmpty, "cannot unfold an empty paper")
    val n = target.length
    val start = enqueueShapes(n, 1, Nil)
    search(start, Set.empty, target)

  def show(instructions: List[Instruction]): String =
    instructions match
      case Nil => ""
      case Instruction(part, index) :: Nil =>
        s"$part $index"
      case Instruction(part, index) :: tail =>
        s"$part $index\n" + show(tail)

  private def search(
      queue: List[(Array[Array[String]], List[Instruction])],
      seen: Set[String],
      target: String
  ): List[Instruction] =
    queue match
      case Nil =>
        throw new IllegalArgumentException(s"cannot unfold $target")
      case (paper, revSteps) :: rest =>
        val key = snapshot(paper)
        if seen.contains(key) then search(rest, seen, target)
        else if isSolved(paper, target) then reverse(revSteps)
        else
          val next = expand(paper, revSteps, candidates(paper))
          search(rest ++ next, seen + key, target)

  private def isSolved(paper: Array[Array[String]], target: String): Boolean =
    Paper.height(paper) == 1 && Paper.width(paper) == 1 && paper(0)(0) == target

  private def enqueueShapes(
      n: Int,
      rows: Int,
      acc: List[(Array[Array[String]], List[Instruction])]
  ): List[(Array[Array[String]], List[Instruction])] =
    if rows > n then reverse(acc)
    else if n % rows == 0 then
      val cols = n / rows
      enqueueShapes(n, rows + 1, (Paper.create(rows, cols), Nil) :: acc)
    else enqueueShapes(n, rows + 1, acc)

  /** Prefer square sheets first so BFS ties pick 2×2 over a longer strip of equal depth. */
  private def expand(
      paper: Array[Array[String]],
      revSteps: List[Instruction],
      ops: List[Instruction]
  ): List[(Array[Array[String]], List[Instruction])] =
    ops match
      case Nil => Nil
      case head :: tail =>
        (Folder.fold(paper, head), head :: revSteps) :: expand(paper, revSteps, tail)

  private def candidates(paper: Array[Array[String]]): List[Instruction] =
    val cols = Paper.width(paper)
    val rows = Paper.height(paper)
    range(1, cols, 'L') ++ range(1, cols, 'R') ++ range(1, rows, 'T') ++ range(1, rows, 'B')

  private def range(from: Int, untilExclusive: Int, part: Char): List[Instruction] =
    if from >= untilExclusive then Nil
    else Instruction(part, from) :: range(from + 1, untilExclusive, part)

  private def snapshot(paper: Array[Array[String]]): String =
    def cells(row: Int, col: Int): String =
      if row >= paper.length then ""
      else if col >= paper(row).length then cells(row + 1, 0)
      else
        val sep = if col == 0 && row > 0 then ";" else if col > 0 then "," else ""
        sep + paper(row)(col) + cells(row, col + 1)
    s"${Paper.height(paper)}x${Paper.width(paper)}:" + cells(0, 0)

  private def reverse[A](list: List[A]): List[A] =
    def go(remaining: List[A], acc: List[A]): List[A] =
      remaining match
        case Nil          => acc
        case head :: tail => go(tail, head :: acc)
    go(list, Nil)
