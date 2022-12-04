package day4

import day0._

class PuzzleDay4 extends PuzzleBase {

  private def makeSets(input: List[String] ) = {
    input.map(line => line.split(','))
      .map(pair => (pair.head.split('-'), pair.last.split('-')))
      .map(pair => (pair._1.head.toInt to pair._1.last.toInt, pair._2.head.toInt to pair._2.last.toInt))
      .map(intervals => (intervals._1.toSet, intervals._2.toSet))
  }

  override def solvePart1(input: List[String]): String = {
    makeSets(input)
      .map(sets => if sets._1.subsetOf(sets._2) || sets._2.subsetOf(sets._1) then 1 else 0)
      .sum.toString
  }
  override def solvePart2(input: List[String]): String = {
    makeSets(input)
      .map(sets => if sets._1.intersect(sets._2) != Set.empty  then 1 else 0)
      .sum.toString
  }
}
