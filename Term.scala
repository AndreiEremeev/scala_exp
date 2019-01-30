package lambda

trait Term {
  def isBelong(variable: Var): Boolean
}

case class Var(name: String) extends Term {
  override def toString(): String = name
  override def isBelong(other: Var): Boolean = (name == other.name)
}

case class Appl(first: Term, second: Term) extends Term {
  override def toString(): String =
    "("+first+" "+second+")"
  override def isBelong(other: Var): Boolean =
    (first.isBelong(other) || second.isBelong(other))
}

case class Abstr(variable: Var, body: Term) extends Term {
  override def toString(): String =
    "λ"+variable+"."+body+""
  override def isBelong(other: Var): Boolean =
    body.isBelong(other)
}
