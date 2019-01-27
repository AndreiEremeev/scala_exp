package lambda



object Number {
  val x = new Var("x")
  val f = new Var("f")
  val n = new Var("n")
  val a = new Var("a")
  val b = new Var("b")
  val Succ = 
    new Abstr(n,
      new Abstr(f,
        new Abstr(x,
          new Appl(
            f,
            new Appl(
              new Appl(n, f),
              x
            )
          )
        )
      )
    )
  val Plus =
    new Abstr(a,
      new Abstr(b,
        new Appl(
          new Appl(a, Succ),
          b,
        )
      )
    )
  def Convert(number: Int): Term = 
    if (number == 0) {
      new Abstr(f, new Abstr(x, x))
    } else {
      new Appl(Succ, Convert(number - 1))
    }
}
