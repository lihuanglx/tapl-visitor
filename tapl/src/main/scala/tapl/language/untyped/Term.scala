package tapl.language.untyped

import tapl.common._
import tapl.component.varapp
import macros.Language

@Language
trait Term[-R, E] extends varapp.Term[R, E] {
  def tmAbs(x: String, e: R): E
}

trait Impl[T] extends Term[Exp[Term], T] {
  override def apply(e: Exp[Term]): T = e(this)
}
