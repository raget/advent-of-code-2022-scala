package day3

import day0._
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using


class PuzzleDay3Tests extends AnyFunSuite {
  val day = "3"
  val puzzleFiles = new PuzzleFiles(day)

  val input1: List[String] = puzzleFiles.getLinesFromInput1
  val exampleInput1: List[String] = puzzleFiles.getLinesFromExampleInput1
  val input2: List[String] = puzzleFiles.getLinesFromInput2
  val exampleInput2: List[String] = puzzleFiles.getLinesFromExampleInput2

  val puzzle = new PuzzleDay3

  test("Get answer for the first example") {
    assert(puzzle.solvePart1(exampleInput1) == "157")
  }

  test("Get answer for the first puzzle") {
    assert(puzzle.solvePart1(input1) == "is the first answer")
  }

  test("Get answer for the second example") {
    assert(puzzle.solvePart2(exampleInput2) == "70")
  }

  test("Get answer for the second puzzle") {
    assert(puzzle.solvePart2(input2) == "is the second answer")
  }
}
