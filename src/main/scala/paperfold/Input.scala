package paperfold

import scala.io.Source
import scala.util.Using

/**
 * Input:
 *   - `FOLD` (or `mod FOLD`) then `length`, `width`, then crease lines (`T 1`, …)
 *   - `UNFOLD` (or `mod UNFOLD`) then the 1×1 stacked sheet (`CABD`)
 */
object Input:
  def readPath(path: String): Job =
    Using.resource(Source.fromFile(path)) { source =>
      parse(source.getLines().toList)
    }

  def parse(lines: List[String]): Job =
    skipBlanks(lines) match
      case Nil =>
        throw new IllegalArgumentException("input is empty")
      case header :: rest =>
        parseMode(header, rest)

  private def skipBlanks(lines: List[String]): List[String] =
    lines match
      case Nil => Nil
      case head :: tail =>
        val trimmed = head.trim
        if trimmed.isEmpty || trimmed.startsWith("#") then skipBlanks(tail)
        else trimmed :: skipBlanks(tail)

  private def parseMode(header: String, rest: List[String]): Job =
    splitWords(header) match
      case "MOD" :: mode :: extra =>
        parseMode((mode :: extra).mkString(" "), rest)
      case "FOLD" :: Nil =>
        parseFold(rest, None, None)
      case "UNFOLD" :: Nil =>
        parseUnfold(rest)
      case "UNFOLD" :: paper =>
        parseUnfold(paper.mkString("") :: rest)
      case _ =>
        throw new IllegalArgumentException(
          s"first attribute must be mod FOLD or mod UNFOLD, got: $header"
        )

  private def parseUnfold(lines: List[String]): Job.Unfold =
    lines match
      case Nil =>
        throw new IllegalArgumentException("UNFOLD requires a folded paper string")
      case paper :: Nil =>
        val folded = paper.trim
        if folded.isEmpty then
          throw new IllegalArgumentException("UNFOLD paper string is empty")
        else Job.Unfold(folded)
      case _ =>
        throw new IllegalArgumentException(
          s"UNFOLD expects a single paper string, got: ${lines.mkString(" / ")}"
        )

  private def parseFold(
      lines: List[String],
      length: Option[Int],
      width: Option[Int]
  ): Job.Fold =
    lines match
      case Nil =>
        Job.Fold(requireSize("length", length), requireSize("width", width), Nil)
      case head :: tail =>
        splitWords(head) match
          case "LENGTH" :: value :: Nil =>
            parseFold(tail, Some(value.toInt), width)
          case "WIDTH" :: value :: Nil =>
            parseFold(tail, length, Some(value.toInt))
          case part :: _ if isInstructionPart(part) =>
            Job.Fold(
              requireSize("length", length),
              requireSize("width", width),
              parseInstructions(lines)
            )
          case _ =>
            throw new IllegalArgumentException(
              s"expected length, width, or a fold instruction, got: $head"
            )

  private def parseInstructions(lines: List[String]): List[Instruction] =
    lines match
      case Nil => Nil
      case head :: tail =>
        splitWords(head) match
          case part :: index :: Nil =>
            Instruction.parse(part, index.toInt) :: parseInstructions(tail)
          case _ =>
            throw new IllegalArgumentException(
              s"instruction must be '<L|R|T|B> <index>', got: $head"
            )

  private def requireSize(name: String, value: Option[Int]): Int =
    value.getOrElse(
      throw new IllegalArgumentException(s"FOLD requires a $name attribute")
    )

  private def isInstructionPart(part: String): Boolean =
    part.length == 1 && "LRTB".contains(part.head)

  private def splitWords(line: String): List[String] =
    line
      .replace("=", " ")
      .replace(":", " ")
      .split("\\s+")
      .toList
      .filter(_.nonEmpty)
      .map(_.toUpperCase)
