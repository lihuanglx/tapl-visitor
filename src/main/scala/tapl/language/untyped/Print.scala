package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Print[A[-R, _]] extends Alg[Exp[A], String] with lambda.Print[A] with varapp.Print[A]

object PrintImpl extends Print[Alg] with Impl[String]
