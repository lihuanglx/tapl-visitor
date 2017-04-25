package tapl.common

trait EParser[A[-X, Y]] extends CommonParser[Exp[A]] {
  val pE: Parser[Exp[A]]
}
