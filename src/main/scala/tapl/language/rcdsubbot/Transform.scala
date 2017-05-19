package tapl.language.rcdsubbot

import tapl.common.Util.E3
import tapl.component.typedrecord
import tapl.language.bot

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with bot.Transform[A, V] with typedrecord.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
