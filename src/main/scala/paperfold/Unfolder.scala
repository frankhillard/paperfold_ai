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
    //println(s"enqueueShapes" + start.length);
    //displayShapes(start)
    val (result, nbfoldcalls) = search(start, Set.empty, target, 0)
    println(s"target nbfoldcalls " + target + ", " + nbfoldcalls);
    result

  def displayShapes(shapes: List[(Array[Array[String]], List[Instruction])]): String = 
     shapes match 
        case Nil => "Finished"
        case (paper, instructions) :: rest => 
          val dispPaper = Paper.show(paper)
          println(dispPaper)
          println(s"inst" + instructions)
          displayShapes(rest)

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
      target: String,
      nbcalls: Int
  ): (List[Instruction], Int) =
    queue match
      case Nil =>
        throw new IllegalArgumentException(s"cannot unfold $target")
      case (paper, revSteps) :: rest =>
        val key = snapshot(paper)
        if seen.contains(key) then search(rest, seen, target, nbcalls)
        else if isSolved(paper, target) then (reverse(revSteps), nbcalls)
        else
          val candidates_pool = candidates(paper)
          val (next, nbfold) = expand(paper, revSteps, candidates_pool, 0)
          //println("expand paper" + Paper.show(paper) + " candidates " + candidates_pool.length + " cost total" + nbcalls + nbfold)
          search(rest ++ next, seen + key, target, nbcalls + nbfold)

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
      ops: List[Instruction],
      nbfold: Int
  ): (List[(Array[Array[String]], List[Instruction])], Int) =
    ops match
      case Nil => (Nil, nbfold)
      case head :: tail =>
        val foldedWithHead = (Folder.fold(paper, head), head :: revSteps)
        val (foldedWithRest, nb) = expand(paper, revSteps, tail, nbfold + 1)  
        (foldedWithHead :: foldedWithRest, nb) 

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
