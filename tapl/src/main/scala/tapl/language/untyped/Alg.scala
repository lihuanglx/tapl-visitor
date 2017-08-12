package tapl.language.untyped

import tapl.common._
import tapl.component.varapp
import macros.Language

@Language
trait Alg[-R, E] extends varapp.Alg[R, E] {
  def tmAbs(x: String, e: R): E
}

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}
