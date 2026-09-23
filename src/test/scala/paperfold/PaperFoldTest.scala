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

  test("R1 on 4x3 yields D C BA; H G FE; L K JI"):
    val paper = Paper.create(3, 4)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('R', 1))
    )
    assertEquals(
      toLists(folded),
      List(
        List("D", "C", "BA"),
        List("H", "G", "FE"),
        List("L", "K", "JI")
      )
    )

  test("R2 on 4x3 yields DA CB; HE GF; LI KJ"):
    val paper = Paper.create(3, 4)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('R', 2))
    )
    assertEquals(
      toLists(folded),
      List(
        List("DA", "CB"),
        List("HE", "GF"),
        List("LI", "KJ")
      )
    )

  test("R2,B2,T1,L1 on 4x3 yields EHLIDABCJKGF"):
    val paper = Paper.create(3, 4)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('R', 2), Instruction('B', 2), Instruction('T', 1), Instruction('L', 1))
    )
    assertEquals(toLists(folded), List(List("EHLIDABCJKGF")))


  test("R2,T1,R1,B1 on 3x3 yields GIHEFCBAD"):
    val paper = Paper.create(3, 3)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('R', 2), Instruction('T', 1), Instruction('R', 1), Instruction('B', 1))
    )
    assertEquals(toLists(folded), List(List("GIHEFCBAD")))

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

  test("FOLD input reads length, width and instructions"):
    val raw = List("mod FOLD", "length 2", "width 2", "T 1", "L 1")
    Input.parse(raw) match
      case Job.Fold(length, width, instructions) =>
        assertEquals(length, 2)
        assertEquals(width, 2)
        assertEquals(instructions, List(Instruction('T', 1), Instruction('L', 1)))
      case other =>
        fail(s"expected FOLD job, got $other")

  test("UNFOLD input reads the stacked paper"):
    assertEquals(Input.parse(List("UNFOLD", "CABD")), Job.Unfold("CABD"))
    assertEquals(Input.parse(List("UNFOLD CABD")), Job.Unfold("CABD"))

  test("UNFOLD CABD recovers T 1 then L 1"):
    assertEquals(
      Unfolder.instructionsFor("CABD"),
      List(Instruction('T', 1), Instruction('L', 1))
    )

  test("UNFOLD of a 3x3 stack round-trips on a 3x3 sheet"):
    val expected = "HIGADFEBC"
    val instructions = Unfolder.instructionsFor(expected)
    val folded = Folder.applyAll(Paper.create(3, 3), instructions)
    assertEquals(toLists(folded), List(List(expected)))

  test("UNFOLD of a 3x3 stack round-trips on a 3x3 sheet"):
    val expected = "GIHEFCBAD"
    val instructions = Unfolder.instructionsFor(expected)
    val folded = Folder.applyAll(Paper.create(3, 3), instructions)
    assertEquals(toLists(folded), List(List(expected)))

  test("UNFOLD of a 4x3 stack round-trips on a 4x3 sheet"):
    val expected = "EHLIDABCJKGF"
    val instructions = Unfolder.instructionsFor(expected)
    val folded = Folder.applyAll(Paper.create(3, 4), instructions)
    assertEquals(toLists(folded), List(List(expected)))

  test("UNFOLD of a 4x3 stack round-trips on a 4x3 sheet"):
    val expected = "FGKJCBADILHE"
    val instructions = Unfolder.instructionsFor(expected)
    val folded = Folder.applyAll(Paper.create(3, 4), instructions)
    assertEquals(toLists(folded), List(List(expected)))

  test("similarity score between GCBIEHADF and HIGADFEBC"):
    val expected = "HIGADFEBC"
    val candidate = "BIEHADFGC"
    val score = UnfolderSmart.similarityScore(candidate.toList, expected.toList, 0)
    assertEquals(score, 4)


  test("UNFOLD SMART of a 3x3 stack round-trips on a 3x3 sheet"):
    val expected = "HIGADFEBC"
    val (instructions, score, (height, width)) = UnfolderSmart.instructionsFor(expected)
    val folded = Folder.applyAll(Paper.create(height, width), instructions)
    assertEquals(toLists(folded), List(List(expected)))


  private def toLists(paper: Array[Array[String]]): List[List[String]] =
    paper.map(_.toList).toList
