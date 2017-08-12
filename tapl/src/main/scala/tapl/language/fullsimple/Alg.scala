package tapl.language.fullsimple

import macros.Language
import tapl.common._
import tapl.component.{typed, extension, variant}

@Language
trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with extension.Alg[R, E, F] with variant.Alg[R, E, F]

@Language
trait TAlg[-F, T] extends typed.TAlg[F, T] with extension.TAlg[F, T] with variant.TAlg[F, T]

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
