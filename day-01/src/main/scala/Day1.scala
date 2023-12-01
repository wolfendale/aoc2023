import cats.data.NonEmptyList

object Day1 {

  def part1(values: String): Int =
    Parser.parse(values)
}

object Parser {

  import atto._
  import Atto._

  def parse(input: String): Int =
    parser.parseOnly(input).done.option.get

  private val newline: Parser[Char] =
    char('\r') | char('\n')

  private val calibrationDigit: Parser[Char] =
    letter.many ~> digit

  private val calibrationNumber: Parser[Int] =
    (calibrationDigit.many1 <~ letter.many).map { chars =>
      s"${chars.head}${chars.last}".toInt
    }

  private val parser: Parser[Int] =
    calibrationNumber.sepBy1(newline).map(_.reduceLeft(_ + _)) <~ endOfInput
}