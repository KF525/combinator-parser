package com.kfulton.combinator.parsers

import cats.implicits._

object ArithParserMain extends App {
  print(ArithParser.expression.run(List('1', '0', '.', '5', '*', '(', '4', '+', '2', ')', '/', '2')))
}
