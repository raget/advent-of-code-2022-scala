package day5

import day0.*

import scala.annotation.tailrec

case class Move(source: Int, target: Int, count: Int)

object CargoCrane {

  def moveCrates(stacks: Array[List[String]], move: Move): Array[List[String]] = {
    val m = Move(move.source - 1, move.target - 1, move.count)
    //for the first puzzle
    // moveCratesRec(stacks, m)
    val newCargoStacks = stacks.indices.toArray.map {
      case s if s == m.source => if stacks(m.source).isEmpty then stacks(m.source).empty else stacks(m.source).drop(m.count)
      case t if t == m.target => stacks(m.source).take(m.count) ::: stacks(m.target)
      case i => stacks(i)
    }
    newCargoStacks
  }

  @tailrec
  private def moveCratesRec(stacks: Array[List[String]], move: Move): Array[List[String]] = {
    move.count match {
      case 0 => stacks
      case i =>
        val stacksAfterMove = CargoCrane.moveOneCrate(stacks, move.source, move.target)
        val nextMove = Move(move.source, move.target, i - 1)
        moveCratesRec(stacksAfterMove, nextMove)
    }
  }

  def top(stacks: Array[List[String]]): String = stacks.map(stack => stack.head).reduce((a, b) => a + b)

  private def moveOneCrate(stacks: Array[List[String]], source: Int, target: Int): Array[List[String]] = {
    val newCargoStacks = stacks.indices.toArray.map {
      case s if s == source => if stacks(source).isEmpty then stacks(source).empty else stacks(source).tail
      case t if t == target => stacks(source).head :: stacks(target)
      case i => stacks(i)
    }
    newCargoStacks
  }
}

class PuzzleDay5 extends PuzzleBase {

  def parseCargo(input: List[String]) = {
    val count = 9 //3 //input.find(line => line.startsWith(" 1")).head.trim.toList.last.toInt not proud on this
    val s1 = input.takeWhile(line => !line.startsWith(" 1"))

    val stacks = (1 to count * 4 by 4).map(i => {
      var r = ""
      for (line <- s1) {
        r += line(i)
      }
      r.trim.toCharArray.toList.map(c => c.toString)
    })
    stacks.toArray
  }

  def parseMoves(input: List[String]): List[Move] = {
    input.dropWhile(line => !line.startsWith("move"))
      .map(l => l.split(" "))
      .map(s => Move(s(3).toInt, s(5).toInt, s(1).toInt))
  }

  override def solvePart1(input: List[String]): String = {
    val cargo = parseCargo(input)
    val moves = parseMoves(input)
    val endState = moves.foldLeft(cargo)((acc: Array[List[String]], next: Move) => CargoCrane.moveCrates(acc, next))
    CargoCrane.top(endState)
  }

  override def solvePart2(input: List[String]): String = {
    solvePart1(input)
  }
}
