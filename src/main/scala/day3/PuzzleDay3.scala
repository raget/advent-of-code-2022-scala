package day3

import day0._

class PuzzleDay3 extends PuzzleBase {

  val lower = ('a' to 'z').zip(1 to 26)
  val upper = ('A' to 'Z').zip(27 to 52)
  val score = lower.concat(upper)

  override def solvePart1(input: List[String]): String = {
    input
      .map(line => (line.take(line.length/2), line.drop(line.length/2)))
      .map(pair => findMisplacedItem(pair._1, pair._2))
      .map(item => score.find(_._1 == item).get._2)
      .sum
      .toString
  }

  def findMisplacedItem(first: String, second: String): Char = {
    first.toCharArray.toSet.intersect(second.toCharArray.toSet).head
  }

  override def solvePart2(input: List[String]): String = {
    val teams = makeTeams(input)
    teams.map(team => team._1.toSet.intersect(team._2.toSet).intersect(team._3.toSet).head)
      .map(item => score.find(_._1 == item).get._2)
      .sum.toString
  }

  def makeTeams(input: List[String]): List[(String, String, String)] = {
    input match
      case Nil => Nil
      case a :: b :: c :: rest => (a, b, c) :: makeTeams(rest)
  }
}
