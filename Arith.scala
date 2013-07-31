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
