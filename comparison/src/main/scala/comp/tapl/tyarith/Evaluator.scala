package comp.tapl.tyarith

// Small-step semantics as described by Pierce
object Evaluator {

  import Util._

  private def eval1(t: Term): Term = t match {
    case TmIf(TmTrue, t2, t3) =>
      t2
    case TmIf(TmFalse, t2, t3) =>
      t3
    case TmIf(t1, t2, t3) =>
      val t11 = eval1(t1)
      TmIf(t11, t2, t3)
    case TmSucc(t1) =>
      val t11 = eval1(t1)
      TmSucc(t11)
    case TmPred(TmZero) =>
      TmZero
    case TmPred(TmSucc(nv1)) if isNumericVal(nv1) =>
      nv1
    case TmPred(t1) =>
      val t2 = eval1(t1)
      TmPred(t2)
    case TmIsZero(TmZero) =>
      TmTrue
    case TmIsZero(TmSucc(nv1)) if isNumericVal(nv1) =>
      TmFalse
    case TmIsZero(t1) =>
      val t2 = eval1(t1)
      TmIsZero(t2)
    case _ => throw new NoRuleApplies(t)
  }

  def eval(t: Term): Term =
    if (isVal(t))
      t
    else {
      val t1 = eval1(t)
      eval(t1)
    }

}

object Typer {
  def typeof(t: Term): Ty = t match {
    case TmTrue =>
      TyBool
    case TmFalse =>
      TyBool
    case TmIf(t1, t2, t3) =>
      if (typeof(t1) == TyBool) {
        val ty2 = typeof(t2)
        if (ty2 == typeof(t3)) {
          ty2
        } else {
          throw new Exception("arms of conditional " + t + " have different types")
        }
      } else {
        throw new Exception("guard of conditional " + t + " is not a boolean")
      }
    case TmZero =>
      TyNat
    case TmSucc(t1) =>
      if (typeof(t1) == TyNat) {
        TyNat
      } else {
        throw new Exception("argument of Succ: is not a number: " + t)
      }
    case TmPred(t1) =>
      if (typeof(t1) == TyNat) {
        TyNat
      } else {
        throw new Exception("argument of Pred: is not a number: " + t)
      }
    case TmIsZero(t1) =>
      if (typeof(t1) == TyNat) {
        TyBool
      } else {
        throw new Exception("argument of IsZero: is not a number: " + t)
      }
  }
}

object Util {
  def isNumericVal(t: Term): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumericVal(t1)
    case _ => false
  }

  def isVal(t: Term): Boolean = t match {
    case TmTrue => true
    case TmFalse => true
    case t if isNumericVal(t) => true
    case _ => false
  }
}

class NoRuleApplies(t: Term) extends Exception("No rule applies for term: " + t)
