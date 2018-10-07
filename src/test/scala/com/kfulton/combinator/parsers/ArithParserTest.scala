package com.kfulton.combinator.parsers

import cats.data.StateT
import cats.implicits._
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class ArithParserTest extends FlatSpec with Matchers with EitherValues {

  type ParseResultOrError[A] = Either[String, A]
  type ParserState[A] = StateT[ParseResultOrError, List[Char], A]

  "anyOf" should "return success if input matches one of valid characters" in {
    val possibleChars = List('a', 'b', 'c')

    val anyOfParser: ParserState[Char] = ArithParser.anyOf(possibleChars)
    anyOfParser.run(List('c', 'd')).right.value shouldBe (List('d'),'c')
  }

  it should "return failure if no matches" in {
    val possibleChars = List('a', 'b', 'c')

    val anyOfParser: ParserState[Char] = ArithParser.anyOf(possibleChars)
    anyOfParser.run(List('d')).left.value shouldBe "Neither parser succeeded."
  }

  "digit" should "succeed if input is a valid number char" in {
    val digitParser = ArithParser.digit
    digitParser.run(List('1')).right.value shouldBe (List(), 1)
  }

  it should "fail if input is not a valid number char" in {
    val digitParser = ArithParser.digit
    digitParser.run(List('a')) shouldBe Left("Neither parser succeeded.")
  }

  "naturalNumber" should "return a complete number" in {
    val naturalNumberParser = ArithParser.naturalNumberParser

    naturalNumberParser.run(List('1', '2','9','0', 'a')).right.value shouldBe (List('a'), 1290)
  }

  it should "return a failure if input is not plus or minus" in {
    val plusOrMinusParser = ArithParser.plusOrMinus

    plusOrMinusParser.run(List('a', '+')).left.value shouldBe "Neither parser succeeded."
  }

  "parenExpression" should "" in {
    val parenExpressionParser = ArithParser.parenExpression

    parenExpressionParser.run(List('(', '2', '+', '5', ')')).right.value shouldBe (List(), 7.0)
  }

  it should "return a failure if any of the subparsers fails" in {
    val parenExpressionParser = ArithParser.parenExpression

    parenExpressionParser.run(List('2', '+', '5', ')')).left.value shouldBe "2 did not satisfy predicate."
    parenExpressionParser.run(List('(', ')')).left.value shouldBe "Neither parser succeeded."
    parenExpressionParser.run(List('(', '2', '+', '5')).left.value shouldBe "No tokens."
    parenExpressionParser.run(List('(', '2', '+', '5', 'a')).left.value shouldBe "a did not satisfy predicate."
  }

  "factor" should "return a natural number or numeric result of an expression" in {
    val factorParser = ArithParser.factor

    factorParser.run(List('2')).right.value shouldBe (List(), 2.0)
    factorParser.run(List('(', '2', ')')).right.value shouldBe (List(), 2.0)
  }

  "term" should "handle subtraction and multiplication" in {
    val termParser = ArithParser.term

    termParser.run(List('2', '0', '*', '4', '/', '1','0')).right.value shouldBe (List(), 8.0)
    termParser.run(List('2','0','/', '4')).right.value shouldBe (List(), 5.0)
    termParser.run(List('2', '0', '/', '4', '*', '1','0')).right.value shouldBe (List(), 50.0)
    termParser.run(List('(','2', '0', ')', '/', '4', '*', '1','0')).right.value shouldBe (List(), 50.0)
  }

  "expression" should "add and subtract numerics correctly" in {
    val expressionParser = ArithParser.expression

    expressionParser.run(List('2', '0', '+', '4')).right.value shouldBe (List(), 24.0)
    expressionParser.run(List('2', '0', '-', '4')).right.value shouldBe (List(), 16.0)
    expressionParser.run(List('2', '0', '+', '4', '+','5')).right.value shouldBe (List(), 29.0)
    expressionParser.run(List('2', '0', '-', '4', '+','5')).right.value shouldBe (List(), 21.0)
    expressionParser.run(List('2', '0', '-', '4', '-','5')).right.value shouldBe (List(), 11.0)
    expressionParser.run(List('2', '0', '+', '4', '-', '1','0')).right.value shouldBe (List(), 14.0)
  }

  it should "compute expressions with addition, subtraction, multiplication and division" in {
    val expressionParser = ArithParser.expression

    expressionParser.run(List('2', '0', '*', '4', '-', '1','0')).right.value shouldBe (List(), 70.0)
    expressionParser.run(List('2','0','/', '4')).right.value shouldBe (List(), 5.0)
    expressionParser.run(List('2', '0', '/', '4', '-', '1','0')).right.value shouldBe (List(), -5.0)
    expressionParser.run(List('2', '0', '/', '3', '+', '1','0')).right.value shouldBe (List(), 16.666666666666668)
    expressionParser.run(List('2', '0', '+', '4', '*', '1','0')).right.value shouldBe (List(), 60.0)
    expressionParser.run(List('2', '0', '+', '4', '/', '2')).right.value shouldBe (List(), 22.0)
  }

  it should "handle expressions with parentheses" in {
    val expressionParser = ArithParser.expression

    expressionParser.run(List('(', '2', '0', '+', '4', ')', '*', '1','0')).right.value shouldBe (List(), 240.0)
    expressionParser.run(List('(', '2', '0', '+', '4', ')', '/', '2')).right.value shouldBe (List(), 12.0)
  }
}