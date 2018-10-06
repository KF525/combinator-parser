package com.kfulton.combinator.parsers

import cats.implicits._

object ArithParserMain extends App {
  print(ArithParser.expression.run(List('1', '+', '2')))
}
