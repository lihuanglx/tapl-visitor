package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon
import tapl.language.fullrecon.Alg.Factory._
import tapl.language.fullrecon.Alg.{Query, Transform}

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[Exp2[A, V], Exp2[A, V], V]
  with recon.Eval[A, V] with let.Eval[A[-?, ?, V]] {

  override def tmUAbs(x: String, e: Exp2[A, V]): Exp2[A, V] = TmUAbs(x, e)

  override def tmApp(e1: Exp2[A, V], e2: Exp2[A, V]): Exp2[A, V] =
    if (e1(isVal))
      if (e2(isVal)) e1 match {
        case TmAbs(x, _, body) => body(subst(x, e2))
        case TmUAbs(x, body) => body(subst(x, e2))
        case _ => typeError()
      } else TmApp[A[-?, ?, V]](e1, apply(e2))
    else TmApp[A[-?, ?, V]](apply(e1), e2)
}

object Eval extends Eval[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val isVal: Alg[Exp2[Alg, Exp[TAlg]], Boolean, Exp[TAlg]] = IsVal

  override def subst(m: Map[String, Exp2[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

trait IsVal[A[-R, E, -F], V] extends Query[Exp2[A, V], Boolean, V] with recon.IsVal[A, V] {
  override def tmUAbs(x: String, e: Exp2[A, V]): Boolean = true
}

object IsVal extends IsVal[Alg, Exp[TAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with recon.Subst[A, V] with let.Subst[A[-?, ?, V]] {

  override def tmUAbs(x: String, e: Exp2[A, V]): Exp2[A, V] = TmUAbs(x, if (m.contains(x)) e else apply(e))
}

class SubstImpl(mp: Map[String, Exp2[Alg, Exp[TAlg]]]) extends Subst[Alg, Exp[TAlg]] with Impl[Exp2[Alg, Exp[TAlg]]] {
  override val m: Map[String, Exp2[Alg, Exp[TAlg]]] = mp
}
