package com.kfulton.combinator.parsers

import cats.implicits._

object ArithParserMain extends App {
  print(ArithParser.execute.run(List('1', '+', '2')))
}
