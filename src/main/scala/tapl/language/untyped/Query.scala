package tapl.language.untyped

import tapl.component._

trait Query[R, T] extends Alg[R, T] with lambda.Query[R, T] with varapp.Query[R, T]
