package tapl.common

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


trait CommonParser[T] extends StandardTokenParsers with PackratParsers {

  lazy val lcid: Parser[String] = ident ^? { case id if id.charAt(0).isLower => id }

  lazy val ucid: Parser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  def parse(inp: String): Option[T]

  def parseBy(parser: Parser[T])(inp: String): Option[T] = {
    val t = phrase(parser)(new lexical.Scanner(inp))
    if (t.successful) Some(t.get) else None
  }

}
