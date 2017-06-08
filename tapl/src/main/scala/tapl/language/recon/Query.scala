package tapl.language.recon

import tapl.component.typed
import tapl.language.tyarith

trait Query[R, E, F] extends Alg[R, E, F] with typed.Query[R, E, F] with tyarith.Query[R, E]
