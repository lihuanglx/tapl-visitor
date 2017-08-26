package play.narrowing


object Common {

  trait SExp[-A[-R, _], -B[-F, _]] {
    def apply[E](alg: A[SExp[B, B], E]): E
  }

  type Exp[-A[-R, _]] = SExp[A, A]

  type Exp2[-A[-R, E, -F], +V] = Exp[A[-?, ?, V]]

  type SExp2[-A[-R, E, -F], -B[-R, E, -F], V1, V2] = SExp[A[-?, ?, V1], B[-?, ?, V2]]

  trait Default[T] {
    val default: T
  }

  trait IIsVal[A[-X, Y]] {
    val isVal: A[Exp[A], Boolean]
  }

}

import Common._


object Bool {

  trait Term[-R, E] {
    def tmTrue(): E

    def tmFalse(): E

    def tmIf(b: R, e1: R, e2: R): E

    def apply(e: R): E
  }

  object Term {

    case class TmTrue[A[-X, Y] <: Term[X, Y], B[-R, _]]() extends SExp[A, B] {
      def apply[E](alg: A[SExp[B, B], E]): E = alg.tmTrue()
    }

    case class TmFalse[A[-X, Y] <: Term[X, Y], B[-R, _]]() extends SExp[A, B] {
      def apply[E](alg: A[SExp[B, B], E]): E = alg.tmFalse()
    }

    case class TmIf[A[-X, Y] <: Term[X, Y], B[-R, _]](e1: SExp[B, B], e2: SExp[B, B], e3: SExp[B, B])
      extends SExp[A, B] {

      def apply[E](alg: A[SExp[B, B], E]): E = alg.tmIf(e1, e2, e3)
    }

    trait Factory {
      type TmTrue[A[-X, Y] <: Term[X, Y], B[-R, _]] = Term.TmTrue[A, B]
      val TmTrue = Term.TmTrue
      type TmFalse[A[-X, Y] <: Term[X, Y], B[-R, _]] = Term.TmFalse[A, B]
      val TmFalse = Term.TmFalse
      type TmIf[A[-X, Y] <: Term[X, Y], B[-R, _]] = Term.TmIf[A, B]
      val TmIf = Term.TmIf
    }

    object Factory extends Factory

    trait Query[-R, E] extends Term[R, E] with Default[E] {
      def tmTrue(): E = default

      def tmFalse(): E = default

      def tmIf(e1: R, e2: R, e3: R): E = default
    }

    trait Narrow[A[-X, Y] <: Term[X, Y]] {
      def narrowToBool[B[-R, _]](e: SExp[A, B]): Option[SExp[Term, B]]
    }

    trait NarrowSelf[B[-R, _]] extends Term[SExp[B, B], Option[SExp[Term, B]]] {
      override def tmTrue(): Option[SExp[Term, B]] = Some(new TmTrue[Term, B]())

      override def tmFalse(): Option[SExp[Term, B]] = Some(new TmFalse[Term, B]())

      override def tmIf(b: SExp[B, B], e1: SExp[B, B], e2: SExp[B, B]): Option[SExp[Term, B]] =
        Some(new TmIf[Term, B](b, e1, e2))
    }

  }

  import Term.Factory._

  trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[SExp[A, A], SExp[A, A]]
    with IIsVal[A] with Term.Narrow[A] {

    override def tmTrue(): SExp[A, A] = new TmTrue[A, A]()

    override def tmFalse(): SExp[A, A] = new TmFalse[A, A]()

    override def tmIf(e1: SExp[A, A], e2: SExp[A, A], e3: SExp[A, A]): SExp[A, A] = {
      if (e1(isVal)) {
        narrowToBool(e1) match {
          case Some(x) => x.apply(new Term[SExp[A, A], SExp[A, A]] {
            override def tmTrue(): SExp[A, A] = e2

            override def tmFalse(): SExp[A, A] = e3

            override def tmIf(b: SExp[A, A], e1: SExp[A, A], e2: SExp[A, A]): SExp[A, A] = sys.error("")

            override def apply(e: SExp[A, A]): SExp[A, A] = sys.error("")
          })
          case _ => sys.error("")
        }
      } else {
        new TmIf[A, A](apply(e1), e2, e3)
      }
    }
  }

  trait IsVal[A[-R, _]] extends Term.Query[SExp[A, A], Boolean] {
    override val default: Boolean = false

    override def tmTrue(): Boolean = true

    override def tmFalse(): Boolean = true
  }


}

object ExtBool {

  trait Term[-R, E] extends Bool.Term[R, E] {
    def tmLit(x: Int): E
  }

  object Term {

    case class TmLit[A[-X, Y] <: Term[X, Y], B[-R, _]](x: Int) extends SExp[A, B] {
      def apply[E](alg: A[SExp[B, B], E]): E = alg.tmLit(x)
    }

    trait Factory extends Bool.Term.Factory {
      type TmLit[A[-X, Y] <: Term[X, Y], B[-R, _]] = Term.TmLit[A, B]
      val TmLit = Term.TmLit
    }

    object Factory extends Factory

    trait Query[-R, E] extends Term[R, E] with Bool.Term.Query[R, E] {
      override def tmLit(x: Int): E = default
    }

    trait Narrow[A[-X, Y] <: Term[X, Y]] {
      def narrowToExtBool[B[-R, _]](e: SExp[A, B]): Option[SExp[Term, B]]
    }

    trait NarrowSelf[B[-R, _]] extends Term[SExp[B, B], Option[SExp[Term, B]]] {
      override def tmLit(x: Int): Option[SExp[Term, B]] = Some(new TmLit[Term, B](x))
    }

    trait NarrowChainBool[A[-X, Y] <: Term[X, Y]] extends Bool.Term.Narrow[A] with Narrow[A] {
      override def narrowToBool[B[-R, _]](e: SExp[A, B]): Option[SExp[Bool.Term, B]] = {
        val t = new Query[SExp[B, B], Option[SExp[Bool.Term, B]]] with Bool.Term.NarrowSelf[B] {
          override val default: Option[SExp[Bool.Term, B]] = None

          override def apply(e: SExp[B, B]): Option[SExp[Bool.Term, B]] = sys.error("")
        }
        narrowToExtBool(e).flatMap(x => x(t))
      }
    }

  }

  import Term.Factory._

  trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[SExp[A, A], SExp[A, A]] with Bool.Eval[A]
    with Term.NarrowChainBool[A] {

    override def tmLit(x: Int): SExp[A, A] = new TmLit[A, A](x)
  }

  trait IsVal[A[-R, _]] extends Term.Query[SExp[A, A], Boolean] with Bool.IsVal[A] {
    override def tmLit(x: Int): Boolean = true
  }

  object Eval extends Eval[Term] {
    override def apply(e: SExp[Term, Term]): SExp[Term, Term] = e(this)

    override val isVal: Term[SExp[Term, Term], Boolean] = new IsVal[Term] {
      override def apply(e: SExp[Term, Term]): Boolean = e(this)
    }

    override def narrowToExtBool[B[-R, _]](e: SExp[Term, B]): Option[SExp[Term, B]] = Some(e)
  }


  def main(args: Array[String]): Unit = {
    val e: SExp[Term, Term] = TmIf[Term, Term](TmTrue(), TmLit[Term, Term](1), TmLit[Term, Term](2))
    println(e(Eval))
  }

}


object Code {

}
