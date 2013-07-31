object Arith {
  type Info = String

  trait Term
  case class True(info: Info) extends Term
  case class False(info: Info) extends Term
  case class If(info: Info, trueB: Term, falseB: Term) extends Term
  case class Zero(info: Info) extends Term
  case class Succ(info: Info, pred: Term) extends Term
  case class Pred(info: Info, succ: Term) extends Term
  case class IsZero(info: Info) extends Term

  def isNumericValue(term: Term) = term match {
    case Zero(_)    => true
    case Succ(_, t) => isNumericValue(t)
    case _          => false
  }

  def isValue(term: Term) = term match {
    case True(_) => true
    case False(_) => true
    case _ if isNumericValue(term) => true
    case _ => false
  }

  case object NoRuleApplies extends Exception("No rule applies.")

  val dummyInfo = "dummy"

  def step(t: Term): Term = match t {
    case If(_, True(_), trueB, _)  => trueB
    case If(_, False(_), _, falseB => falseB
    case If(fi, guard, trueB, falseB) =>
      val guardN = step(guard)
      If(info, guardN, trueB, falseB)
    case Succ(info, pred)
      val predN = step(pred)
      Succ(info, predN)
    case Pred(_, Zero(_)) =>
      Zero(dummyInfo)
    case Pred(_, Succ(_, pred)) if isNumericValue(pred) =>
      pred
    case Pred(info, succ) =>
      val succN = step(succ)
      Pred(info, succ)
    case Zero(_, Zero(_)) =>
      True(dummyInfo)
    case IsZero(_, Succ(_, pred) if isNumericValue(pred) =>
        TmFalse(dummyInfo)
    case IsZero(info, nat) =>
      val natN = step(nat)
      IsZero(info, nat)
    case _ => throw NoRuleApplies
  }

  def eval(term: Term): Term = try {
    val termS = step(term)
    eval(termS)
  } catch {
    case NoRuleApplies => term
  }
}

