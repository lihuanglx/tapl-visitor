package tapl.language.simplebool

import tapl.component.{typed, typedbool}

trait Query[R, E, F] extends Alg[R, E, F] with typed.Query[R, E, F] with typedbool.Alg.Query[R, E]
