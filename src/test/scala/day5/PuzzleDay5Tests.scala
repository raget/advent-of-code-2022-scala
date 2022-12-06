package day5

import day0._
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using


class PuzzleDay5Tests extends AnyFunSuite {
  val day = "5"
  val puzzleFiles = new PuzzleFiles(day)

  val input1: List[String] = puzzleFiles.getLinesFromInput1
  val exampleInput1: List[String] = puzzleFiles.getLinesFromExampleInput1
  val input2: List[String] = puzzleFiles.getLinesFromInput2
  val exampleInput2: List[String] = puzzleFiles.getLinesFromExampleInput2

  val puzzle = new PuzzleDay5

  test("Top") {
    val stacks = Array[List[String]](List("D", "N", "Z"), List("C", "M"), List("P"))
    assert(CargoCrane.top(stacks)== "DCP")
  }

  test("Move one crate") {
    val stacks = Array[List[String]](List("D", "N", "Z"), List("C", "M"), List("P"))
    val move = Move(1, 2, 1)
    val stacksAfterMove = CargoCrane.moveCrates(stacks, move)
    assert(CargoCrane.top(stacksAfterMove) == "NDP")
  }
  //    [D]
  //    [N][C]
  //    [Z][M][P]
  //     1  2  3
  test("Move two crates") {
    val stacks = Array[List[String]](List("D", "N", "Z"), List("C", "M"), List("P"))
    val move = Move(1, 2, 2)
    val cargoAfterMove = CargoCrane.moveCrates(stacks, move)
    assert(CargoCrane.top(cargoAfterMove) == "ZNP")
  }

  test("Get answer for the first example") {
    assert(puzzle.solvePart1(exampleInput1) == "CMZ")
  }

  test("Get answer for the first puzzle") {
    assert(puzzle.solvePart1(input1) == "is the first answer")
  }

  test("Get answer for the second example") {
    assert(puzzle.solvePart2(exampleInput2) == "MCD")
  }

  test("Get answer for the second puzzle") {
    assert(puzzle.solvePart2(input2) == "is the second answer")
  }
}
