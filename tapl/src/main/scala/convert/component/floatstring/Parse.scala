package convert.component.floatstring

import convert.common._
import convert.component.floatstring.Term._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.delimiters += "*"

  // todo
  lazy val pFloatStringE: Parser[Exp[A]] =
    chainl1(pE, "*" ^^^ { (e1: Exp[A], e2: Exp[A]) => TmTimes[A, A](e1, e2) }) |||
      stringLit ^^ TmString[A, A]
}