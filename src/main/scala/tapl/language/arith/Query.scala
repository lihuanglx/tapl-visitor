package tapl.language.arith

import tapl.common.Exp
import tapl.component.{bool, nat}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with bool.Query[A, T] with nat.Query[A, T]
