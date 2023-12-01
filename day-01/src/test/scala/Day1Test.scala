import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day1Test extends AnyFreeSpec with Matchers {

  private val testInput1 =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  private val testInput2 =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must succeed for the example input" in {
      Day1.part1(testInput1) mustEqual 142
    }

    "must succeed for the test input" in {
      Day1.part1(input) mustEqual 54927
    }
  }

  "part 2" - {

    "must succeed for the example input" in {
      Day1.part2(testInput2) mustEqual 281
    }

    "must succeed for the test input" in {
      Day1.part2(input) mustEqual 0
    }
  }
}
