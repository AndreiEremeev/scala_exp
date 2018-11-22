trait Term {
  def toString(): String
  def isBelong(variable: Var): Boolean
}

case class Var(name: String) extends Term {
  override def toString(): String = name
  override def isBelong(other: Var): Boolean = (name == other.name)
}

case class Appl(first: Term, second: Term) extends Term {
  override def toString(): String =
    "("+first.toString()+" "+second.toString()+")"
  override def isBelong(other: Var): Boolean =
    (first.isBelong(other) || second.isBelong(other))
}

case class Abstr(variable: Var, body: Term) extends Term {
  override def toString(): String =
    "λ"+variable.toString()+".("+body.toString()+")"
  override def isBelong(other: Var): Boolean =
    body.isBelong(other)
}

object Reducer {
  def changeOfVariable(expression: Term, varOperand: Var, expressionOperand: Term): Term =
    expression match {
      case Var(name) => {
        if (name == varOperand.name)
          expressionOperand
        else
          expression
      }
      case Appl(first, second) => {
        new Appl(
          changeOfVariable(first, varOperand, expressionOperand),
          changeOfVariable(second, varOperand, expressionOperand)
        )
      }
      case Abstr(variable, body) => {
        if (variable.name == varOperand.name) {
          expression
        } else {
          if (expressionOperand.isBelong(variable)) {
            val newVar = new Var(variable.name+"`")
            new Abstr(
              newVar,
              changeOfVariable(
                changeOfVariable(body, variable, newVar),
                varOperand,
                expressionOperand
              )
            )
          } else {
            new Abstr(variable, changeOfVariable(body, varOperand, expressionOperand))
          }
        }
      }
  }

  def alphaConversion(abstr : Abstr, variable : Var): Abstr =
    new Abstr(
      variable, 
      changeOfVariable(abstr.body, abstr.variable, variable)
    )

}

object Debug {
  def alphaConversionString(abstr : Abstr, variable : Var): String = abstr.toString()+" α => "+Reducer.alphaConversion(abstr, variable).toString()
  def changeOfVarString(expression: Term, varOperand: Var, expressionOperand: Term): String = "["+expressionOperand.toString()+"/"+varOperand.toString()+"]"+expression.toString() +" => "+Reducer.changeOfVariable(expression, varOperand, expressionOperand)
  
}

object Main {
  def main(args: Array[String]) {
    val x = new Var("x")
    val y = new Var("y")
    val z = new Var("z")
    val t = new Var("t")
    val bigTerm = new Abstr(x,new Abstr(y, new Abstr(z, new Appl(x,new Appl(y,z)))))
    println(bigTerm.toString())
    println(Reducer.alphaConversion(bigTerm, t).toString())
    println(Reducer.alphaConversion(bigTerm, z).toString())
  }
}
