package tapl.language.arith

import tapl.component.{bool, nat}

trait Query[R, T] extends Alg[R, T] with bool.Query[R, T] with nat.Query[R, T]
