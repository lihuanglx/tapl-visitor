package tapl.common

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


trait CommonParser[T] extends StandardTokenParsers with PackratParsers {

  def parse(inp: String): Option[T]

  def parseBy(parser: Parser[T])(inp: String): Option[T] = {
    val t = phrase(parser)(new lexical.Scanner(inp))
    if (t.successful) Some(t.get) else None
  }

}
