package paperfold

import scala.io.Source
import scala.util.Using

/** Input: first line `rows cols`, then one instruction per line (`L 2`, `T 1`, …). */
object Input:
  def readPath(path: String): (Int, Int, List[Instruction]) =
    Using.resource(Source.fromFile(path)) { source =>
      parse(source.getLines().toList)
    }

  def parse(lines: List[String]): (Int, Int, List[Instruction]) =
    val meaningful = skipBlanks(lines)
    meaningful match
      case Nil =>
        throw new IllegalArgumentException("input is empty")
      case header :: rest =>
        val (rows, cols) = parseSize(header)
        (rows, cols, parseInstructions(rest))

  private def skipBlanks(lines: List[String]): List[String] =
    lines match
      case Nil => Nil
      case head :: tail =>
        val trimmed = head.trim
        if trimmed.isEmpty || trimmed.startsWith("#") then skipBlanks(tail)
        else trimmed :: skipBlanks(tail)

  private def parseSize(header: String): (Int, Int) =
    header.split("\\s+").toList match
      case rows :: cols :: Nil => (rows.toInt, cols.toInt)
      case _ =>
        throw new IllegalArgumentException(
          s"first line must be '<rows> <cols>', got: $header"
        )

  private def parseInstructions(lines: List[String]): List[Instruction] =
    lines match
      case Nil => Nil
      case head :: tail =>
        head.split("\\s+").toList match
          case part :: index :: Nil =>
            Instruction.parse(part, index.toInt) :: parseInstructions(tail)
          case _ =>
            throw new IllegalArgumentException(
              s"instruction must be '<L|R|T|B> <index>', got: $head"
            )
