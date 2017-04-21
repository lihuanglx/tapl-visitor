package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Query[A[-R, _], T]  extends Alg[Exp[A], T] with lambda.Query[A, T] with varapp.Query[A, T]
