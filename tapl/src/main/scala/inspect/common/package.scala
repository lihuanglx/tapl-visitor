package inspect

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

package object common {

  trait SExp[-A[-R, _], -B[-F, _]] {
    def apply[E](alg: A[SExp[B, B], E]): E
  }

  type Exp[-A[-R, _]] = SExp[A, A]

  type Exp2[-A[-R, E, -F], +V] = Exp[A[-?, ?, V]]

  type Exp3[-A[-R, E, -T, -K], +V1, +V2] = Exp[A[-?, ?, V1, V2]]

  type SExp2[-A[-R, E, -F], -B[-R, E], +V] = SExp[A[-?, ?, V], B[-?, ?]]

  type SExp3[-A[-R, E, -T, -K], B[-R, E], +V1, +V2] = SExp[A[-?, ?, V1, V2], B]

  trait Default[T] {
    val default: T
  }

  def typeError(msg: String = "Type error!"): Nothing = sys.error(msg)

  type CtxTo[A[-X, Y]] = Ctx[String, Exp[A]] => Exp[A]

  implicit def constType[A[-X, Y]](t: Exp[A]): CtxTo[A] = _ => t

  class Ctx[K, V](m: Map[K, V]) {
    def +(b: (K, V)): Ctx[K, V] = new Ctx(m + b)

    def apply(k: K): V = m.getOrElse(k, typeError())

    override def toString: String = m.toString
  }

  object Ctx {
    def empty[K, V](): Ctx[K, V] = new Ctx[K, V](Map())
  }

  trait IIsVal[A[-X, Y]] {
    val isVal: A[Exp[A], Boolean]
  }

  trait ISubst[A[-R, _]] {
    def subst(x: String, e: Exp[A]): A[Exp[A], Exp[A]] = subst(Map(x -> e))

    def subst(m: Map[String, Exp[A]]): A[Exp[A], Exp[A]]
  }

  trait SubstAux[A[-R, _]] {
    val m: Map[String, Exp[A]]
  }

  trait ITEq[A[-X, Y]] {
    val tEquals: Exp[A] => Exp[A] => Boolean
  }

  trait ISubtypeOf[A[-X, Y]] {
    val subtypeOf: A[Exp[A], Exp[A] => Boolean]
  }

  trait IJoin[A[-X, Y]] {
    val join: A[Exp[A], Exp[A] => Exp[A]]
  }

  trait IMeet[A[-X, Y]] {
    val meet: A[Exp[A], Exp[A] => Exp[A]]
  }

  trait IRecEq[A[-X, Y]] {
    val recEq: A[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  }

  trait JoinAux[A[-X, Y]] extends IMeet[A] with ISubtypeOf[A] {
    def directJoin(t1: Exp[A], t2: Exp[A]): Option[Exp[A]] =
      if (t1(subtypeOf)(t2)) Some(t2) else if (t2(subtypeOf)(t1)) Some(t1) else None
  }

  trait MeetAux[A[-X, Y]] extends IJoin[A] with ISubtypeOf[A] {
    def directMeet(t1: Exp[A], t2: Exp[A]): Option[Exp[A]] =
      if (t1(subtypeOf)(t2)) Some(t1) else if (t2(subtypeOf)(t1)) Some(t2) else None
  }

  // parsing
  trait CommonParser extends StandardTokenParsers with PackratParsers {
    type Parser[E] = PackratParser[E]

    lazy val lcid: Parser[String] = ident ^? { case id if id.charAt(0).isLower => id }

    lazy val ucid: Parser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

    def parseBy[T](parser: Parser[T])(inp: String): Option[T] = {
      val t = phrase(parser)(new lexical.Scanner(inp))
      if (t.successful)
        Some(t.get)
      else {
        println(t.toString)
        None
      }
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

  trait KParser[A[-X, Y]] extends CommonParser {
    val pK: Parser[Exp[A]]

    def parseK(inp: String): Option[Exp[A]] = parseBy(pK)(inp)
  }

  trait ETParser[A[-R, E, -F], B[-F, T]] extends EParser[A[-?, ?, Exp[B]]] with TParser[B]

}
