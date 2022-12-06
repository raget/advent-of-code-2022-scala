package day6

import day0._

class PuzzleDay6 extends PuzzleBase {

  val window = 14 //4

  override def solvePart1(input: List[String]): String = {
    routine(input.head, window).toString
  }

  def routine(input: String, pos: Int): Int = {
    if (input.length < window) {
      throw new Exception("not found")
    }
    if input.take(window).distinct.length == window then
      pos
    else
      routine(input.drop(1), pos + 1)
  }

  override def solvePart2(input: List[String]): String = {
    val window = 14
    routine(input.head, window).toString
  }
}
