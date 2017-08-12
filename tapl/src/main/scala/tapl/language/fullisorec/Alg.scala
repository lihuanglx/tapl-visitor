package tapl.language.fullisorec

import macros.Language
import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

@Language
trait Alg[-R, E, -F] extends fullsimple.Alg[R, E, F] {
  def tmFold(e: R, t: F): E

  def tmUnfold(e: R, t: F): E
}

@Language
trait TAlg[-F, T] extends fullsimple.TAlg[F, T] with rectype.TAlg[F, T]

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
