package com.kfulton.combinator.parsers

import cats.data.StateT
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class ParserTest extends FlatSpec with Matchers {

  type ParseResultOrError[A] = Either[String, A]
  type ParserState[A] = StateT[ParseResultOrError, List[Char], A]

  "chainl" should "return the result of the expression" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser: ParserState[String] = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val chainParser = Parser.chainl[String](stringParser, constantParser)
    chainParser.run(List('a', '+', 'a', '+', 'a')) shouldBe Right(List(), "aaa")
  }

  it should "handle just a single value" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser: ParserState[String] = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val result = Parser.chainl[String](stringParser, constantParser)
    result.run(List('a')) shouldBe Right(List(), "a")
  }

  it should "stop appropriately" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser: ParserState[String] = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val chainParser = Parser.chainl[String](stringParser, constantParser)
    chainParser.run(List('a', '+', 'a', '+', 'a', 'b')) shouldBe Right(List('b'), "aaa")
  }
  
  "constant" should "return a parser with the correct replacement" in {
    val add = (a: Double, b: Double) => a + b
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val charParser = Parser.satisfies(plus)

    val constantParser = Parser.constant(charParser, add)
    constantParser.run(List('+')) shouldBe Right(List(), add)
  }

  it should "return the correct int value" in {
    val one = 1
    val oneF: Char => Boolean = (c: Char) => c.equals('1')
    val charParser = Parser.satisfies(oneF)

    val constantParser = Parser.constant(charParser, one)
    constantParser.run(List('1')) shouldBe Right(List(), one)
  }

  "oneOrMore" should "take a specified letter continuously" in {
    val singleAParser: ParserState[String] = Parser.string("a")

    val manyAsParser = Parser.oneOrMore[String](singleAParser)
    manyAsParser.run(List('a', 'a', 'a', 'b')) shouldBe Right(List('b'), List("a","a","a"))
  }

  "zeroOrOne" should "return Some('a') if the parser succeeds" in {
    val stringParser: ParserState[String] = Parser.string("a")

    Parser.zeroOrOne(stringParser).run(List('a')) shouldBe Right(List(), Some("a"))
  }

  it should "return None if the parser fails" in {
    val stringParser: ParserState[String] = Parser.string("a")

    Parser.zeroOrOne(stringParser).run(List('b')) shouldBe Right(List('b'), None)
  }

  "map" should "transform result type" in {
    val aParser = Parser.string("a")

    val mapParser = Parser.map[String, String](x => x*2)(aParser)
    mapParser.run(List('a')) shouldBe Right(List(), "aa")
  }

  it should "fail if it is unable to map successfully" in {
    val aParser = Parser.string("b")

    val mapParser = Parser.map[String, String](x => x*2)(aParser)
    mapParser.run(List('a')) shouldBe Left("")
  }

  "orElse" should "return new parser if one parser is valid" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val orParser1 = Parser.orElse[String](aParser, bParser)
    orParser1.run(List('a', 'c')) shouldBe Right(List('c'), "a")
    val orParser2 = Parser.orElse[String](bParser,aParser)
    orParser2.run(List('a', 'c')) shouldBe Right(List('c'), "a")
  }

  it should "return failure if neither parser is valid" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val orParser1 = Parser.orElse[String](aParser,bParser)
    orParser1.run(List('c', 'a')) shouldBe Left("")
  }

  "andThen" should "return a new parsers if both are successful" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val andThenParser = Parser.andThen(aParser,bParser)
    andThenParser.run(List('a','b','c')) shouldBe Right(List('c'), List("a","b"))
  }

  it should "return a failure if only one parser is valid" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val andThenParser = Parser.andThen(aParser,bParser)
    andThenParser.run(List('a','c', 'b')) shouldBe Left("")
    andThenParser.run(List('c','b', 'a')) shouldBe Left("")
  }

  it should "return a failure if neither parser is valid" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val andThenParser = Parser.andThen(aParser,bParser)
    andThenParser.run(List('c','c', 'b')) shouldBe Left("")
  }

  "satisfies" should "return a parser if the current char matches the value" in {
    val plus = (c: Char) => c.equals('+')
    val charParser = Parser.satisfies(plus)

    charParser.run(List('+')) shouldBe Right(List(), '+')
  }

  it should "return a failure if the current char does not match the value" in {
    val plus = (c: Char) => c.equals('+')
    val charParser = Parser.satisfies(plus)

    charParser.run(List('-')) shouldBe Left("")
  }

  "string" should "match a single char" in {
    val aParser: ParserState[String] = Parser.string("a")

    aParser.run(List('a')) shouldBe Right(List(), "a")
    aParser.run(List('a', 'a')) shouldBe Right(List('a'), "a")
    aParser.run(List('a', 'b')) shouldBe Right(List('b'), "a")
    aParser.run(List('a', 'b', 'c')) shouldBe Right(List('b', 'c'), "a")
    aParser.run(List('b', 'a')) shouldBe Left("")
  }
}