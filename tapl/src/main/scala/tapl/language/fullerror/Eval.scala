package tapl.language.fullerror

import tapl.common._
import tapl.component.typedbool
import tapl.language.bot
import tapl.language.fullerror.Alg.{Query, Transform}
import tapl.language.fullerror.Alg.Factory._

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[Exp2[A, V], Exp2[A, V], V]
  with bot.Eval[A, V] with typedbool.Eval[A[-?, ?, V]] {

  override def tmError(): Exp2[A, V] = TmError[A, V]()

  override def tmTry(e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] = e1 match {
    case TmError() => e2
    case _ => if (e1(isVal)) e1 else TmTry[A, V](apply(e1), e2)
  }

  override def tmApp(e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] = (e1, e2) match {
    case (TmError(), _) => e1
    case (_, TmError()) => e2
    case _ => super.tmApp(e1, e2)
  }

  override def tmIf(e1: Exp2[A, V], e2: Exp2[A, V], e3: Exp2[A, V]): Exp2[A, V] = e1 match {
    case TmError() => TmError[A, V]()
    case _ => super.tmIf(e1, e2, e3)
  }
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val isVal: Alg[Exp2[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, Exp2[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V]
  with bot.IsVal[A, V] with typedbool.IsVal[A[-?, ?, V]] {

  override def tmError(): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with bot.Subst[A, V]

class SubstImpl(mp: Map[String, Exp2[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val m: Map[String, Exp2[Alg, Exp[TAlg]]] = mp
}
