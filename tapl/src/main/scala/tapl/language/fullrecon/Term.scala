package tapl.language.fullrecon

import macros.Language
import tapl.common._
import tapl.component.let
import tapl.language.recon

@Language
trait Term[-R, E, -F] extends recon.Term[R, E, F] with let.Term[R, E] {
  def tmUAbs(x: String, e: R): E
}

@Language
trait Type[-F, T] extends recon.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}

