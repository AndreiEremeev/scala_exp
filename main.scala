trait Term {
  def toString(): String
  def isBelong(variable: Var): Boolean
}

case class Var(name: String) extends Term {
  override def toString(): String = name
  override def isBelong(other: Var): Boolean = (name == other.name)
}

case class Appl(first: Term, second: Term) extends Term {
  override def toString(): String = "("+first.toString()+" "+second.toString()+")"
  override def isBelong(other: Var): Boolean = (first.isBelong(other) || second.isBelong(other))
}

case class Abstr(variable: Var, body: Term) extends Term {
  override def toString(): String = "λ"+variable.toString()+".("+body.toString()+")"
  override def isBelong(other: Var): Boolean = body.isBelong(other)
}

object Reducer {
  def changeOfVariable(expression: Term, varOperand: Var, expressionOperand: Term): Term = expression match {
    case Var(name) => if (name == varOperand.name ) expressionOperand else expression
    case Appl(first, second) => new Appl(changeOfVariable(first, varOperand, expressionOperand), changeOfVariable(second, varOperand, expressionOperand))
    case Abstr(variable, body) => 
      if (variable.name == varOperand.name) {
        expression
      } else {
        if (expressionOperand.isBelong(variable)) {
          val newVar = new Var(variable.name+"`")
          new Abstr(newVar, changeOfVariable(changeOfVariable(body, variable, newVar), varOperand, expressionOperand))
        } else {
          new Abstr(variable, changeOfVariable(body, varOperand, expressionOperand))
        }
      }
  }

  def alphaConversion(abstr : Abstr, variable : Var): Abstr = new Abstr(variable, changeOfVariable(abstr.body, abstr.variable, variable))

  def changeOfVarString(expression: Term, varOperand: Var, expressionOperand: Term): String = {
    "["+expressionOperand.toString()+"/"+varOperand.toString()+"]"+expression.toString()+" => "+changeOfVariable(expression, varOperand, expressionOperand)
  }
  def alphaConversionString(abstr : Abstr, variable : Var): String = abstr.toString()+" α => "+alphaConversion(abstr, variable).toString()
}
object HelloWorld {
  def main(args: Array[String]) {
    val tru = new Abstr(new Var("x"),new Abstr(new Var("y"), new Var("x")) )
    println("TRUE == "+tru.toString())
    val fls = new Abstr(new Var("x"),new Abstr(new Var("y"), new Var("y")) )
    println("FALSE == "+fls.toString())
    val simpleVar = new Var("x")
    val simpleAppl = new Appl(new Var("x"), new Var("x"))
    val simpleAbstr = new Abstr(new Var("x"), new Var("x"))
    println(Reducer.changeOfVarString(simpleVar, new Var("x"), new Var("y")))
    println(Reducer.changeOfVarString(simpleAbstr, new Var("x"), new Var("y")))
    println(Reducer.changeOfVarString(simpleAppl, new Var("x"), new Var("y")))
    println(Reducer.changeOfVarString(tru, new Var("x"), new Var("z")))
    println(Reducer.changeOfVarString(tru, new Var("y"), new Var("z")))
    println(Reducer.alphaConversionString(tru, new Var("z")))
    println(Reducer.alphaConversionString(tru, new Var("y")))
  }
}
