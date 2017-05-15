package tapl.language.fulluntyped

import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

trait Query[R, T] extends Alg[R, T] with arith.Query[R, T] with untyped.Query[R, T]
  with floatstring.Query[R, T] with let.Query[R, T] with record.Query[R, T]
