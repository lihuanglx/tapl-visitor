package tapl.language.arith

import tapl.common._
import tapl.component._

trait Print[A[-R, _]] extends Alg[Exp[A], String] with bool.Print[A] with nat.Print[A]

object Print extends Print[Alg] with Impl[String]