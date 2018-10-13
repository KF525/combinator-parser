package com.kfulton.combinator.parsers

import cats.data.StateT
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import cats.implicits._

class ParserTest extends FlatSpec with Matchers with EitherValues {

  type ParseResultOrError[A] = Either[String, A]
  type ParserState[A] = StateT[ParseResultOrError, List[Char], A]

  "chainl" should "return the result of the expression" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser: ParserState[String] = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val chainParser = Parser.chainl[String](stringParser, constantParser)
    chainParser.run(List('a', '+', 'a', '+', 'a')).right.value shouldBe (List(), "aaa")
  }

  it should "handle just a single value" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser: ParserState[String] = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val result = Parser.chainl[String](stringParser, constantParser)
    result.run(List('a')).right.value shouldBe (List(), "a")
  }

  it should "stop appropriately" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParser: ParserState[String] = Parser.string("a")
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val chainParser = Parser.chainl[String](stringParser, constantParser)
    chainParser.run(List('a', '+', 'a', '+', 'a', 'b')).right.value shouldBe (List('b'), "aaa")
  }

  it should "chain in the correct order" in {
    def add: (String, String) => String = (a:String, b: String) => a ++ b

    val stringParserA: ParserState[String] = Parser.string("a")
    val stringParserB: ParserState[String] = Parser.string("b")
    val orElseParser = Parser.orElse(stringParserA, stringParserB)
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val constantParser = Parser.constant(Parser.satisfies(plus), add)

    val chainParser = Parser.chainl[String](orElseParser, constantParser)
    chainParser.run(List('a', '+', 'b', '+', 'a', '+', 'b', '+', 'a')).right.value shouldBe (List(), "ababa")
  }

  "map" should "transform result type" in {
    val aParser = Parser.string("a")

    val mapParser = Parser.map[String, String](x => x*2)(aParser)
    mapParser.run(List('a')).right.value shouldBe (List(), "aa")
  }

  it should "fail if it is unable to map successfully" in {
    val aParser = Parser.string("b")

    val mapParser = Parser.map[String, String](x => x*2)(aParser)
    mapParser.run(List('a')).left.value shouldBe "Expected b but found a."
  }

  "constant" should "return a parser with the correct replacement" in {
    val add = (a: Double, b: Double) => a + b
    val plus: Char => Boolean = (c: Char) => c.equals('+')
    val charParser = Parser.satisfies(plus)

    val constantParser = Parser.constant(charParser, add)
    constantParser.run(List('+')).right.value shouldBe (List(), add)
  }

  it should "return the correct int value" in {
    val one = 1
    val oneF: Char => Boolean = (c: Char) => c.equals('1')
    val charParser = Parser.satisfies(oneF)

    val constantParser = Parser.constant(charParser, one)
    constantParser.run(List('1')).right.value shouldBe (List(), one)
  }

  "oneOrMore" should "take a specified letter continuously" in {
    val singleAParser: ParserState[String] = Parser.string("a")

    val manyAsParser = Parser.oneOrMore[String](singleAParser)
    manyAsParser.run(List('a', 'a', 'a', 'b')).right.value shouldBe (List('b'), List("a","a","a"))
  }

  "zeroOrOne" should "return Some('a') if the parser succeeds" in {
    val stringParser: ParserState[String] = Parser.string("a")

    Parser.zeroOrOne(stringParser).run(List('a')).right.value shouldBe (List(), Some("a"))
  }

  it should "return None if the parser fails" in {
    val stringParser: ParserState[String] = Parser.string("a")

    Parser.zeroOrOne(stringParser).run(List('b')).right.value shouldBe (List('b'), None)
  }

  "andThen" should "return a new parsers if both are successful" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val andThenParser = Parser.andThen(aParser,bParser)
    andThenParser.run(List('a','b','c')).right.value shouldBe (List('c'), List("a","b"))
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

  "orElse" should "return new parser if one parser is valid" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val orParser1 = Parser.orElse[String](aParser, bParser)
    orParser1.run(List('a', 'c')).right.value shouldBe (List('c'), "a")
    val orParser2 = Parser.orElse[String](bParser,aParser)
    orParser2.run(List('a', 'c')).right.value shouldBe (List('c'), "a")
  }

  it should "return failure if neither parser is valid" in {
    val aParser = Parser.string("a")
    val bParser = Parser.string("b")

    val orParser1 = Parser.orElse[String](aParser,bParser)
    orParser1.run(List('c', 'a')).left.value shouldBe "Neither parser succeeded."
  }

  "satisfies" should "return a parser if the current char matches the value" in {
    val plus = (c: Char) => c.equals('+')
    val charParser = Parser.satisfies(plus)

    charParser.run(List('+')).right.value shouldBe (List(), '+')
  }

  it should "return a failure if the current char does not match the value" in {
    val plus = (c: Char) => c.equals('+')
    val charParser = Parser.satisfies(plus)

    charParser.run(List('-')).left.value shouldBe "- did not satisfy predicate."
    charParser.run(List()).left.value shouldBe "No tokens."
  }

  "string" should "match a single char" in {
    val aParser: ParserState[String] = Parser.string("a")

    aParser.run(List('a')).right.value shouldBe (List(), "a")
    aParser.run(List('a', 'a')).right.value shouldBe (List('a'), "a")
    aParser.run(List('a', 'b')).right.value shouldBe (List('b'), "a")
    aParser.run(List('a', 'b', 'c')).right.value shouldBe (List('b', 'c'), "a")
    aParser.run(List('b', 'a')).left.value shouldBe "Expected a but found b."
    aParser.run(List('b')).left.value shouldBe "Expected a but found b."
    aParser.run(List()).left.value shouldBe "No tokens."
  }

  "pure" should "always succeed with the designated value and without consuming input" in {
    val pureParser = Parser.pure('5')

    pureParser.run(List('1')).right.value shouldBe (List('1'), '5')
    pureParser.run(List()).right.value shouldBe (List(), '5')
  }
}