package com.kfulton.combinator.parsers

import cats.data.StateT
import cats.implicits._

object Parser {

  type ParseResultOrError[A] = Either[String, A]
  type ParserState[A] = StateT[ParseResultOrError, List[Char], A]

  def chainl[A](p1: ParserState[A], p2: ParserState[(A, A) => A]): ParserState[A] =
    for {
      num1 <- p1
      maybeOp <- zeroOrOne(p2)
      //maybeNum <- zeroOrOne(p1)
      maybeNum <- zeroOrOne(chainl(p1, p2))
    } yield (maybeOp, maybeNum) match {
      case (Some(op), Some(num)) => op(num, num1) //chainl(Parser.pure(op(num, num1)), p2)
      case _ => num1
    }

  def flatMap[A,B](p1: ParserState[A], f: A => ParserState[B]): ParserState[B] =
    p1.flatMap(f)

  def map[A, B](f: A => B)(p1: ParserState[A]): ParserState[B] =
    p1.map(f)

  def constant[A, B](p1: ParserState[A], value: B): ParserState[B] =
    for {
      char <- p1
    } yield value

  def oneOrMore[A](p1: ParserState[A]): ParserState[List[A]] =
    for {
      a <- p1
      maybeA <- zeroOrOne(oneOrMore(p1))
    } yield maybeA match {
      case Some(a2) => a::a2
      case _ => List(a)
    }

  def zeroOrOne[A](p1: ParserState[A]): ParserState[Option[A]] =
    StateT[ParseResultOrError, List[Char], Option[A]] {
      chars =>
        p1.run(chars) match {
          case Right((chars1, a1)) => Right((chars1, Some(a1)))
          case Left("") => Right(chars, None)
        }
    }

  def andThen[A](p1: ParserState[A], p2: ParserState[A]): ParserState[List[A]] =
    StateT[ParseResultOrError, List[Char], List[A]] {
      chars =>
        val e1: ParseResultOrError[(List[Char], A)] = p1.run(chars)
        e1 match {
          case (Right((chars1, a1))) =>
            val e2: ParseResultOrError[(List[Char], A)] = p2.run(chars1)
            e2 match {
              case (Right((chars2, a2))) => Right((chars2, List(a1, a2)))
              case Left("") => Left("")
            }
          case Left("") => Left("")
        }
    }

  def orElse[A](p1: ParserState[A], p2: ParserState[A]): ParserState[A] =
    StateT[ParseResultOrError, List[Char], A] {
      chars =>
        val e1: ParseResultOrError[(List[Char], A)] = p1.run(chars)
        lazy val e2: ParseResultOrError[(List[Char], A)] = p2.run(chars)
        e1 match {
          case Right((chars1, a1)) => Right((chars1, a1))
          case _ => e2 match {
            case Right((chars2, a2)) => Right((chars2, a2))
            case _ => Left("")
          }
        }
    }

  def satisfies(predicate: Char => Boolean): ParserState[Char] =
    StateT[ParseResultOrError, List[Char], Char] {
      case c::chars if predicate(c) => Right(chars, c)
      case _ => Left("")
    }

  def string(value: String): ParserState[String] =
    StateT[ParseResultOrError, List[Char], String] {
      case c::chars if c.toString.equals(value) => Right(chars, c.toString)
      case _ => Left("")
    }

  def pure[A](a: A): ParserState[A] =
    StateT[ParseResultOrError, List[Char], A](chars => Right(chars, a))
}