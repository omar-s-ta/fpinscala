package fpinscala.exercises.parsing

import fpinscala.exercises.testing.Prop
import fpinscala.exercises.testing.Gen
import scala.util.matching.Regex
import java.util.regex.Pattern

trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A])

  object Laws:
    private def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p(0)(0), p(0)(1), p(1))
    private def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p(0), p(1)(0), p(1)(1))

    def equal[A](p1: Parser[A], p2: Parser[A])(generator: Gen[String]): Prop =
      Prop.forAll(generator)(str => p1.run(str) == p2.run(str))

    def mapLaw[A](p: Parser[A])(generator: Gen[String]): Prop =
      equal(p, p.map(a => a))(generator)

    def succeedLaw[A](p: Parser[A])(generator: Gen[(A, String)]): Prop =
      Prop.forAll(generator)((a, str) => succeed(a).run(str) == Right(a))

    def productAssociativityLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(generator: Gen[String]): Prop =
      equal(((pa ** pb) ** pc).map(unbiasL), (pa ** (pb ** pc)).map(unbiasR))(generator)

    def mapProductLaw[A, B, C, D](pa: Parser[A], pb: Parser[B])(f: A => C, g: B => D)(generator: Gen[String]): Prop =
      equal(pa.map(f) ** pb.map(g), (pa ** pb).map((a, b) => (f(a), g(b))))(generator)

  end Laws

  def string(s: String): Parser[String]
  def regex(r: Regex): Parser[String]
  def fail(msg: String): Parser[Nothing]
  def succeed[A](a: A): Parser[A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def defaultSucceed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def eof: Parser[String] = regex("\\z".r)
  def whitespace: Parser[String] = regex("\\s*".r)
  def digits: Parser[String] = regex("\\d+".r)

  def doubleString: Parser[String] =
    regex("[+-]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  def double: Parser[Double] =
    doubleString.map(_.toDouble)

  def to(s: String): Parser[String] =
    regex((".*?" + Pattern.quote(s)).r)

  def quoted: Parser[String] =
    string("\"") *> to("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] =
    (string("\"") *> rawString).map(escaped)

  private def rawString: Parser[String] =
    to("\"").flatMap: raw =>
      if endsWithEscapedQuote(raw) then rawString.map(raw + _)
      else succeed(raw.dropRight(1))

  private def endsWithEscapedQuote(raw: String): Boolean =
    val str = raw.dropRight(1)
    str.reverse.takeWhile(_ == '\\').length % 2 == 1

  private def escaped(raw: String): String =
    val sb = new StringBuilder
    var at = 0
    while at < raw.length do
      if raw(at) == '\\' && at + 1 < raw.length then
        raw(at + 1) match
          case '"' =>
            sb += '"'
            at += 2
          case '\\' =>
            sb += '\\'
            at += 2
          case '/' =>
            sb += '/'
            at += 2
          case 'b' =>
            sb += '\b'
            at += 2
          case 'f' =>
            sb += '\f'
            at += 2
          case 'n' =>
            sb += '\n'
            at += 2
          case 'r' =>
            sb += '\r'
            at += 2
          case 't' =>
            sb += '\t'
            at += 2
          case other =>
            sb += other
            at += 2
      else
        sb += raw(at)
        at += 1
    sb.toString

  extension [A](p: Parser[A])
    def attempt: Parser[A]
    def slice: Parser[String]
    def flatMap[B](f: A => Parser[B]): Parser[B]
    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def map2[B, C](other: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.flatMap(a => other.map(b => f(a, b)))

    def *>[B](other: => Parser[B]): Parser[B] =
      p.slice.map2(other)((_, b) => b)

    def <*(other: => Parser[Any]): Parser[A] =
      p.map2(other.slice)((a, _) => a)

    def as[B](b: B): Parser[B] =
      p.slice.map(_ => b)

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(List.empty[A])

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def many1: Parser[List[A]] = oneOrMany
    def oneOrMany: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    infix def or(other: => Parser[A]): Parser[A]
    def |(other: Parser[A]): Parser[A] = p.or(other)

    def product[B](other: => Parser[B]): Parser[(A, B)] =
      p.flatMap(a => other.map(b => (a, b)))

    def **[B](other: => Parser[B]): Parser[(A, B)] = product(other)

    def foldLeft(op: Parser[(A, A) => A]): Parser[A] =
      p.map2((op ** p).many)((h, t) => t.foldLeft(h)((acc, b) => b(0)(acc, b(1))))

    def sep(separator: Parser[Any]): Parser[List[A]] =
      p.sep1(separator) | succeed(Nil)

    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    def opt: Parser[Option[A]] =
      p.map(Some(_)) | succeed(None)

    def token: Parser[A] =
      p.attempt <* whitespace

    def root: Parser[A] =
      p <* eof

    def run(input: String): Either[ParseError, A]

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next()
    else ""

case class ParseError(stack: List[(Location, String)] = List(), otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] =
    for
      str <- regex("[0-9]+".r)
      n <- str.toIntOption match
        case Some(value) => succeed(value)
        case None => fail("expected an integer")
    yield n

  val nConsecutiveAs: Parser[Int] =
    for
      n <- nonNegativeInt
      _ <- char('a').listOfN(n)
    yield n
