package tapl.language.rcdsubbot

import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with bot.Alg.Transform[A, V] with typedrecord.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
