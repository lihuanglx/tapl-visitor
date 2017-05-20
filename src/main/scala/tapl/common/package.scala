package tapl

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

package object common {

  // Exp definition
  trait Exp[-A[-R, _]] {
    def apply[E](alg: A[Exp[A], E]): E
  }

  type E3[-A[-R, E, -F], +V] = Exp[({type lam[-X, Y] = A[X, Y, V]})#lam]

  //
  trait Default[T] {
    val default: T
  }

  trait PrintT[T] {
    def printT(t: T): String
  }

  // type checker and evaluation
  def typeError(msg: String = "Type error!"): Nothing = sys.error(msg)

  type Type[A[-X, Y]] = Context[String, Exp[A]] => Exp[A]

  class Context[K, V](m: Map[K, V]) {
    def +(b: (K, V)): Context[K, V] = new Context(m + b)

    def apply(k: K): V = m(k)
  }

  object Context {
    def empty[K, V](): Context[K, V] = new Context[K, V](Map())
  }

  trait EvalAux[A[-X, Y]] {
    val isVal: A[Exp[A], Boolean]
  }

  trait EvalSubst[A[-R, _]] extends EvalAux[A] {
    val subst: (String, Exp[A]) => A[Exp[A], Exp[A]]
  }

  trait SubstAux[A[-R, _]] {
    val x: String
    val e: Exp[A]
  }

  trait TyperAux[A[-X, Y]] {
    val tEquals: A[Exp[A], Exp[A] => Boolean]

    implicit def constType(t: Exp[A]): Type[A] = _ => t
  }

  trait TyperAuxSub[A[-X, Y]] extends TyperAux[A] {
    val subtypeOf: A[Exp[A], Exp[A] => Boolean]

    def join(t1: Exp[A], t2: Exp[A]): Exp[A] =
      if (t1(subtypeOf)(t2)) t2 else if (t2(subtypeOf)(t1)) t1 else typeError()
  }

  // parsing
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

    def parse(inp: String): Option[Exp[A]] = parseBy(pE)(inp)
  }

  trait TParser[A[-X, Y]] extends CommonParser {
    val pT: Parser[Exp[A]]

    def parseT(inp: String): Option[Exp[A]] = parseBy(pT)(inp)
  }

  trait ETParser[A[-R, E, -F], B[-F, T]] extends EParser[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam] with TParser[B]

}
