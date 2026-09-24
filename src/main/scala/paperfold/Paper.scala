package paperfold

/** Recursively built grid of paper cells. Each cell is a stack of layer names (a String). */
object Paper:
  /** Row-major naming: top-left is A, then B to the right, wrapping to the next row. */
  def create(rows: Int, cols: Int): Array[Array[String]] =
    // all printable ascii Char
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+,-./0123456789:;<=>?@[]^_{|}~"
    require(rows > 0 && cols > 0, "paper must have positive size")
    def fillRow(row: Int, col: Int): Array[String] =
      if col >= cols then Array.empty
      else Array(cellName(row * cols + col)) ++ fillRow(row, col + 1)
    def fill(row: Int): Array[Array[String]] =
      if row >= rows then Array.empty
      else Array(fillRow(row, 0)) ++ fill(row + 1)
    fill(0)

  def cellName(index: Int): String =
    require(index >= 0)
    def letters(n: Int): String =
      val digit = ('A' + (n % 26)).toChar.toString
      if n < 26 then digit
      else letters(n / 26 - 1) + digit
    letters(index)

  def height(paper: Array[Array[String]]): Int = paper.length

  def width(paper: Array[Array[String]]): Int =
    if paper.isEmpty then 0 else paper.head.length

  def show(paper: Array[Array[String]]): String =
    def rowString(row: Int): String =
      if row >= paper.length then ""
      else
        val line = showRow(paper(row), 0)
        if row == paper.length - 1 then line
        else line + "\n" + rowString(row + 1)
    def showRow(row: Array[String], col: Int): String =
      if col >= row.length then ""
      else if col == row.length - 1 then row(col)
      else row(col) + " | " + showRow(row, col + 1)
    rowString(0)
