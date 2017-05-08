package tapl.language.tyarith

import tapl.component.{typedbool, typednat}

trait Query[R, T] extends Alg[R, T] with typedbool.Query[R, T] with typednat.Query[R, T]
