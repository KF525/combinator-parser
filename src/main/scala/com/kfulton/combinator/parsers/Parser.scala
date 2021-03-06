package com.kfulton.combinator.parsers

import cats.data.StateT
import cats.implicits._

object Parser {

  type ParseResultOrError[A] = Either[String, A]
  type Parser[A] = StateT[ParseResultOrError, List[Char], A]
  val noTokensMessage = "No tokens."

  private def chainRemainder[A](a1: A, p1: Parser[A], p2: Parser[(A,A) => A]): Parser[A] =
    for {
      maybeOp <- zeroOrOne(p2)
      maybeA <- zeroOrOne(p1)
      result <- (maybeOp, maybeA) match {
        case (Some(op), Some(a)) => chainRemainder(op(a, a1), p1, p2)
        case _ => pure(a1)
      }
    } yield result

  def chainl[A](p1: Parser[A], p2: Parser[(A, A) => A]): Parser[A] =
    for {
      a <- p1
      result <- chainRemainder(a, p1, p2)
    } yield result

  def flatMap[A,B](p1: Parser[A], f: A => Parser[B]): Parser[B] =
    p1.flatMap(f)

  def map[A, B](f: A => B)(p1: Parser[A]): Parser[B] =
    p1.map(f)

  def constant[A, B](p1: Parser[A], value: B): Parser[B] =
    for {
      char <- p1
    } yield value

  def oneOrMore[A](p1: Parser[A]): Parser[List[A]] =
    for {
      a <- p1
      maybeA <- zeroOrOne(oneOrMore(p1))
    } yield maybeA match {
      case Some(a2) => a::a2
      case _ => List(a)
    }

  def zeroOrOne[A](p1: Parser[A]): Parser[Option[A]] =
    StateT[ParseResultOrError, List[Char], Option[A]] {
      chars =>
        p1.run(chars) match {
          case Right((chars1, a1)) => Right((chars1, Some(a1)))
          case _ => Right(chars, None)
        }
    }

  def andThen[A](p1: Parser[A], p2: Parser[A]): Parser[List[A]] =
    StateT[ParseResultOrError, List[Char], List[A]] {
      chars =>
        val e1: ParseResultOrError[(List[Char], A)] = p1.run(chars)
        e1 match {
          case (Right((chars1, a1))) =>
            val e2: ParseResultOrError[(List[Char], A)] = p2.run(chars1)
            e2 match {
              case (Right((chars2, a2))) => Right((chars2, List(a1, a2)))
              case _ => Left("")
            }
          case _ => Left("")
        }
    }

  def orElse[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
    StateT[ParseResultOrError, List[Char], A] {
      chars =>
        val e1: ParseResultOrError[(List[Char], A)] = p1.run(chars)
        lazy val e2: ParseResultOrError[(List[Char], A)] = p2.run(chars)
        e1 match {
          case Right((chars1, a1)) => Right((chars1, a1))
          case _ => e2 match {
            case Right((chars2, a2)) => Right((chars2, a2))
            case _ => Left(s"Neither parser succeeded.")
          }
        }
    }

  def satisfies(predicate: Char => Boolean): Parser[Char] =
    StateT[ParseResultOrError, List[Char], Char] {
      case c::chars if predicate(c) => Right(chars, c)
      case c::_ if !predicate(c) => Left(s"$c did not satisfy predicate.")
      case _ => Left(noTokensMessage)
    }

  def string(value: String): Parser[String] =
    StateT[ParseResultOrError, List[Char], String] {
      case c::chars if c.toString.equals(value) => Right(chars, c.toString)
      case c::_ if !c.toString.equals(value) => Left(s"Expected $value but found $c.")
      case _ => Left(noTokensMessage)
    }

  def char(value: Char): Parser[Char] =
    satisfies(c => c.equals(value))

  def pure[A](a: A): Parser[A] =
    StateT[ParseResultOrError, List[Char], A](chars => Right(chars, a))
}