import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day1Test extends AnyFreeSpec with Matchers {

  private val testInput =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  private val input = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must succeed for the example input" in {
      Day1.part1(testInput) mustEqual 142
    }

    "must succeed for the test input" in {
      Day1.part1(input) mustEqual 0
    }
  }
}
