package paperfold

sealed trait Job

object Job:
  /** Fold a blank `length`×`width` sheet with the given crease list. */
  final case class Fold(length: Int, width: Int, instructions: List[Instruction]) extends Job

  /** Recover a crease list that produces this 1×1 stacked sheet. */
  final case class Unfold(folded: String) extends Job
