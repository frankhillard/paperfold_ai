package paperfold

class PaperFoldTest extends munit.FunSuite:

  test("2x2 paper is filled row-major from A"):
    val paper = Paper.create(2, 2)
    assertEquals(toLists(paper), List(List("A", "B"), List("C", "D")))

  test("3x3 paper with L 2, B 1, R 1, T 1 should be HIGADFEBC"):
    val paper = Paper.create(3, 3)
    assertEquals(
      toLists(paper),
      List(
        List("A", "B", "C"),
        List("D", "E", "F"),
        List("G", "H", "I")
      )
    )
    val folded = Folder.applyAll(
      paper,
      List(Instruction('L', 2), Instruction('B', 1), Instruction('R', 1), Instruction('T', 1))
    )
    assertEquals(toLists(folded), List(List("HIGADFEBC")))


  test("4x3 paper continues naming on the next row"):
    val paper = Paper.create(3, 4)
    assertEquals(
      toLists(paper),
      List(
        List("A", "B", "C", "D"),
        List("E", "F", "G", "H"),
        List("I", "J", "K", "L")
      )
    )

  test("T 1 then L 1 on 2x2 yields CABD"):
    val paper = Paper.create(2, 2)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('T', 1), Instruction('L', 1))
    )
    assertEquals(toLists(folded), List(List("CABD")))

  test("L 2 on 4 columns folds 0 onto 3 and 1 onto 2"):
    val paper = Paper.create(3, 4)
    val folded = Folder.fold(paper, Instruction('L', 2))
    assertEquals(
      toLists(folded),
      List(
        List("BC", "AD"),
        List("FG", "EH"),
        List("JK", "IL")
      )
    )

  test("R 3 on 4 columns folds column 3 over column 2"):
    val paper = Paper.create(3, 4)
    val folded = Folder.fold(paper, Instruction('R', 3))
    assertEquals(
      toLists(folded),
      List(
        List("A", "B", "DC"),
        List("E", "F", "HG"),
        List("I", "J", "LK")
      )
    )

  test("folding flips an already stacked cell"):
    val afterTop = Folder.fold(Paper.create(2, 2), Instruction('T', 1))
    assertEquals(toLists(afterTop), List(List("AC", "BD")))
    val afterLeft = Folder.fold(afterTop, Instruction('L', 1))
    assertEquals(toLists(afterLeft), List(List("CABD")))

  test("input file parser reads size and instructions"):
    val raw = List("2 2", "T 1", "L 1")
    val (rows, cols, instructions) = Input.parse(raw)
    assertEquals(rows, 2)
    assertEquals(cols, 2)
    assertEquals(instructions, List(Instruction('T', 1), Instruction('L', 1)))

  private def toLists(paper: Array[Array[String]]): List[List[String]] =
    paper.map(_.toList).toList
