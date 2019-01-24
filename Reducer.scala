package lambda

object Reducer {
  def changeOfVariable(expression: Term, varOperand: Var, expressionOperand: Term): (Term, Boolean) =
    expression match {
      case Var(name) => {
        if (name == varOperand.name)
          (expressionOperand, true)
        else
          (expression, false)
      }
      case Appl(first, second) => {
        val (newFirst, fOk) = changeOfVariable(first, varOperand, expressionOperand)
        val (newSecond, sOk) = changeOfVariable(second, varOperand, expressionOperand)
        (new Appl(newFirst, newSecond), fOk || sOk)
      }
      case Abstr(variable, body) => {
        if (variable.name == varOperand.name) {
          (expression, false)
        } else {
          if (expressionOperand.isBelong(variable)) {
            val newVar = new Var(variable.name+"`")
            val newAbstr = alphaConversion(new Abstr(variable, body), newVar)
            changeOfVariable(newAbstr, varOperand, expressionOperand)
          } else {
            val (newBody, ok) = changeOfVariable(body, varOperand, expressionOperand)
            (new Abstr(variable, newBody), ok)
          }
        }
      }
  }

  def alphaConversion(abstr: Abstr, variable: Var): Abstr = {
    val (newBody, _) = changeOfVariable(abstr.body, abstr.variable, variable)
    new Abstr(variable, newBody)
  }

  def betaConversion(abstr: Abstr, term: Term): (Term, Boolean) =
    changeOfVariable(abstr.body, abstr.variable, term)

  def reduceOnce(term: Term): (Term, Boolean) =
    term match {
      case Appl(first, second) => {
        first match {
          case a: Abstr =>
            betaConversion(a, second)
          case default =>
            val (newFirst, fOk) = reduceOnce(first)
            if (fOk) {
              (new Appl(newFirst, second), true)
            } else {
              val (newSecond, sOk) = reduceOnce(second)
              (new Appl(newFirst, newSecond), sOk)
            }
        }
      }
      case Abstr(variable, body) => {
        val (newBody, ok) = reduceOnce(body)
        (new Abstr(variable, newBody), ok)
      }
      case Var(variable) => {
        (term, false)
      }
    }
  
  def Reduce(term: Term): Term = {
    val (tmpTerm: Term, ok) = reduceOnce(term)
    println(term)
    if (ok) {
      Reduce(tmpTerm)
    } else {
      tmpTerm
    }
  }
}
