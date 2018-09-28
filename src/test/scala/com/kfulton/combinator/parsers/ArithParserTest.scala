package com.kfulton.combinator.parsers

import cats.data.StateT
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

class ArithParserTest extends FlatSpec with Matchers {

  type ParseResultOrError[A] = Either[String, A]
  type ParserState[A] = StateT[ParseResultOrError, List[Char], A]

  "anyOf" should "return success if input matches one of valid characters" in {
    val possibleChars = List('a', 'b', 'c')

    val anyOfParser: ParserState[Char] = ArithParser.anyOf(possibleChars)
    anyOfParser.run(List('c', 'd')) shouldBe Right((List('d'),'c'))
  }

  it should "return failure if no matches" in {
    val possibleChars = List('a', 'b', 'c')

    val anyOfParser: ParserState[Char] = ArithParser.anyOf(possibleChars)
    anyOfParser.run(List('d')) shouldBe Left("")
  }

  "digit" should "succeed if input is a valid number char" in {
    val digitParser = ArithParser.digit
    digitParser.run(List('1')) shouldBe Right(List(), 1)
  }

  it should "fail if input is not a valid number char" in {
    val digitParser = ArithParser.digit
    digitParser.run(List('a')) shouldBe Left("")
  }

  "completeDigit" should "return a complete number" in {
    val entireDigitParser = ArithParser.naturalNumberParser

    entireDigitParser.run(List('1', '2','9','0', 'a')) shouldBe Right(List('a'), 1290)
  }

  //TODO: Trouble testing because the functions are different objects
//  "plusOrMinus" should "return success if input is plus or minus" in {
//    val plusOrMinusParser = ArithParser.plusOrMinus
//
//    plusOrMinusParser.run(List('+', 'a')) shouldBe Right(List('a'), '+')
//    plusOrMinusParser.run(List('-', 'a')) shouldBe Right(List('a'), '-')
//  }≠

  it should "return a failure if input is not plus or minus" in {
    val plusOrMinusParser = ArithParser.plusOrMinus

    plusOrMinusParser.run(List('a', '+')) shouldBe Left("")
  }

  "execute" should "add and subtract numerics correctly" in {
    val executeParser = ArithParser.execute

    //executeParser.run(List('2', '0', '+', '4')) shouldBe Right(List(), 24.0)
    //executeParser.run(List('2', '0', '-', '4')) shouldBe Right(List(), 16.0)
    //executeParser.run(List('2', '0', '+', '4', '+','5')) shouldBe Right(List(), 29.0)
    executeParser.run(List('2', '0', '-', '4', '-','5')) shouldBe Right(List(), 11.0)
    //executeParser.run(List('2', '0', '+', '4', '-', '1','0')) shouldBe Right(List(), 14.0)
  }
}