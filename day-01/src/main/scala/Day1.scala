import atto.Parser
import cats.data.NonEmptyList

import cats._
import cats.implicits._

object Day1 {

  def part1(input: String): Int =
    Part1Parser.parse(input)

  def part2(input: String): Int =
    Part2Parser.parse(input)
}

object Part1Parser {

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

object Part2Parser {

  import atto._
  import Atto._

  def parse(input: String): Int =
    parser.parseOnly(input).done.option.get

  def lookahead[A](parser: Parser[A]): Parser[A] =
    (pos ~ parser ~ pos).flatMap { case ((start, value), end) =>
      advance(-(end - start)) >| value
    }

  private val newline: Parser[Char] =
    char('\r') | char('\n')

  private val wordDigit: Parser[Char] =
    lookahead(
      digit |
      (string("zero") >| '0') |
      (string("one") >| '1') |
      (string("two") >| '2') |
      (string("three") >| '3') |
      (string("four") >| '4') |
      (string("five") >| '5') |
      (string("six") >| '6') |
      (string("seven") >| '7') |
      (string("eight") >| '8') |
      (string("nine") >| '9')
    ) <~ advance(1)

  private lazy val unwantedLetters: Parser[Unit] =
    manyUntil(letter, lookahead(wordDigit)).void

  private val calibrationDigit: Parser[Char] =
    unwantedLetters ~> wordDigit

  private val calibrationNumber: Parser[Int] =
    (calibrationDigit.many1 <~ letter.many).map { chars =>
      s"${chars.head}${chars.last}".toInt
    }

  private val parser: Parser[Int] =
    calibrationNumber.sepBy1(newline).map(_.reduceLeft(_ + _)) <~ endOfInput
}