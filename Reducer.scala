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
        (Appl(newFirst, newSecond), fOk || sOk)
      }
      case Abstr(variable, body) => {
        if (variable.name == varOperand.name) {
          (expression, false)
        } else {
          val newVar = newRandVar(expressionOperand, variable)
          val newAbstr = alphaConversion(Abstr(variable, body), newVar)
          val (newBody, ok) = changeOfVariable(newAbstr.body, varOperand, expressionOperand)
          (Abstr(newVar, newBody), ok)
        }
      }
  }
  def newRandVar(expression: Term, oldVar: Var): Var = {
    if (!expression.isBelong(oldVar)) {
      return oldVar
    }
    Var(oldVar+"`")
  }

  def alphaConversion(abstr: Abstr, variable: Var): Abstr = {
    if (abstr.variable.name == variable.name) {
      return abstr
    }
    val (newBody, _) = changeOfVariable(abstr.body, abstr.variable, variable)
    Abstr(variable, newBody)
  }

  def betaConversion(abstr: Abstr, term: Term): (Term, Boolean) = {
    val (newTerm, _) = changeOfVariable(abstr.body, abstr.variable, term)
    return (newTerm, true)
  }

  def reduceOnce(term: Term): (Term, Boolean) =
    term match {
      case Appl(first, second) => {
        first match {
          case a: Abstr =>
            betaConversion(a, second)
          case default =>
            val (newFirst, fOk) = reduceOnce(first)
            if (fOk) {
              (Appl(newFirst, second), true)
            } else {
              val (newSecond, sOk) = reduceOnce(second)
              (Appl(newFirst, newSecond), sOk)
            }
        }
      }
      case Abstr(variable, body) => {
        val (newBody, ok) = reduceOnce(body)
        (Abstr(variable, newBody), ok)
      }
      case Var(variable) => {
        (term, false)
      }
    }
  
  def Reduce(term: Term): Term = {
    val (tmpTerm: Term, ok) = reduceOnce(term)
    if (ok) {
      Reduce(tmpTerm)
    } else {
      tmpTerm
    }
  }
}
