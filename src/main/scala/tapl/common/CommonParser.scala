package tapl.common

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait CommonParser extends StandardTokenParsers with PackratParsers {
  type Parser[E] = PackratParser[E]

  lazy val lcid: Parser[String] = ident ^? { case id if id.charAt(0).isLower => id }

  lazy val ucid: Parser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  def parseBy[T](parser: Parser[T])(inp: String): Option[T] = {
    val t = phrase(parser)(new lexical.Scanner(inp))
    if (t.successful) Some(t.get) else None
  }
}

trait EParser[A[-X, Y]] extends CommonParser {
  val pE: Parser[Exp[A]]
}

trait TParser[A[-X, Y]] extends CommonParser {
  val pT: Parser[Exp[A]]
}

trait ETParser[A[-R, E], B[-F, T]] extends EParser[A] with TParser[B]
