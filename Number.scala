package lambda



object Number {
  val x = Var("x")
  val f = Var("f")
  val n = Var("n")
  val a = Var("a")
  val b = Var("b")
  val Succ = 
    Abstr(n,
      Abstr(f,
        Abstr(x,
          Appl(
            f,
            Appl(
              Appl(n, f),
              x
            )
          )
        )
      )
    )
  val Plus =
    Abstr(a,
      Abstr(b,
        Appl(
          Appl(a, Succ),
          b,
        )
      )
    )
  def Convert(number: Int): Term = 
    if (number == 0) {
      Abstr(f, Abstr(x, x))
    } else {
      Appl(Succ, Convert(number - 1))
    }
}
