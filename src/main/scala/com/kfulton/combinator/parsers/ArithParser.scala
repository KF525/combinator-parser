package com.kfulton.combinator.parsers

import cats.data.StateT
import cats.implicits._

object ArithParser {
  type ParseResultOrError[A] = Either[String, A]
  type ParserState[A] = StateT[ParseResultOrError, List[Char], A]

  def execute =
    Parser.chainl[Double](ArithParser.naturalNumberParser, ArithParser.plusOrMinus)

  def anyOf(possibleMatches: List[Char]): ParserState[Char] = {
    val possibleChars: List[ParserState[Char]] = for {
      ch <- possibleMatches
    } yield Parser.satisfies((c: Char) => c.equals(ch))
    possibleChars.reduce(Parser.orElse[Char])
  }

  def plusOrMinus: ParserState[(Double, Double) => Double] =
    Parser.map[Char, (Double, Double) => Double](
      c => if (c.equals('+')) (a: Double, b: Double) => a + b
      else (a: Double, b: Double) => b - a)(anyOf(List('+', '-')))

  def multOrDivide: ParserState[(Double, Double) => Double] =
    Parser.map[Char, (Double, Double) => Double](
      c => if (c.equals('*')) (a: Double, b: Double) => a * b
      else (a: Double, b: Double) => a / b)(anyOf(List('*', '/')))


  def naturalNumberParser: ParserState[Double] =
    Parser.oneOrMore[Double](ArithParser.digit).map(joinDigits)


  def digit: ParserState[Double] =
    Parser.map[Char, Double](_.toString.toDouble)(anyOf(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')))


  def joinDigits(ints: List[Double]): Double =
    ints.zipWithIndex.map { case (int, index) => val exp = (ints.length - 1) - index
      int * scala.math.pow(10,exp)}.sum
}