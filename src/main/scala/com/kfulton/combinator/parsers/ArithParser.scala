package com.kfulton.combinator.parsers

import cats.implicits._

object ArithParser {
  import Parser._

  def expression: Parser[Double] =
    Parser.chainl[Double](term, orElse(
        operation(char('+'), (a: Double, b: Double) => a + b),
        operation(char('-'), (a: Double, b: Double) => b - a)))

  def term: Parser[Double] =
    Parser.chainl[Double](factor, orElse(
      operation(char('*'), (a: Double, b: Double) => a * b),
      operation(char('/'), (a: Double, b: Double) => b / a)))

  def factor: Parser[Double] =
    Parser.orElse(decimal, parenExpression)

  def parenExpression: Parser[Double] =
    for {
      _ <- char('(')
      expr <- expression
      _ <- char(')')
    } yield expr

  def anyOf(possibleMatches: List[Char]): Parser[Char] = {
    val possibleChars: List[Parser[Char]] = for {
      ch <- possibleMatches
    } yield Parser.satisfies((c: Char) => c.equals(ch))
    possibleChars.reduce(Parser.orElse[Char])
  }

  def operation(char: Parser[Char], op: (Double, Double) => Double) =
    for {
     c <- char
    } yield op

  def doubles: Parser[List[Double]] =
    Parser.oneOrMore[Double](ArithParser.digit)

  def decimal: Parser[Double] =
    for {
      left <- doubles
      _ <- zeroOrOne(char('.'))
      right <- zeroOrOne(doubles)
    } yield generateDecimalNumber(left,right)

  def digit: Parser[Double] =
    Parser.map[Char, Double](_.toString.toDouble)(anyOf(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')))

  def generateDecimalNumber(left: List[Double], right: Option[List[Double]]) = {
    val rightD = right match {
      case Some(d) =>
        val decIndices = d.indices.map(i => -(i + 1))
        generateNumber(d, decIndices.toList)
      case None => .0
    }
    val leftIndices = left.indices.reverse
    generateNumber(left, leftIndices.toList) + rightD
  }

  def generateNumber(doubles: List[Double], indices: List[Int]): Double = {
    doubles.zip(indices).map {case (int, index) => int * scala.math.pow(10, index)}.sum
  }
}