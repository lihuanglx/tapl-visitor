package tapl.language.fulluntyped

import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

trait Print[A[-R, _]] extends Alg[Exp[A], String] with arith.Print[A] with untyped.Print[A]
  with floatstring.Print[A] with let.Print[A] with record.Print[A]

object Print extends Print[Alg] with Impl[String]
