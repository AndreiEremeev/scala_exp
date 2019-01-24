import lambda._

object Main extends App {
  override def main(args: Array[String]) {
    val x = new Var("x")
    val y = new Var("y")
    val z = new Var("z")
    val t = new Var("t")
    val something1 = new Appl(new Abstr(x, x), y)
    val something2 = new Appl(new Abstr(x, new Abstr(y, new Appl(x, y))), y)
    val something3 = new Appl(new Abstr(x, new Appl(x, x)), new Abstr(x, new Appl(x, x)))
    Reducer.Reduce(something1)
    Reducer.Reduce(something2)
    Reducer.Reduce(something3)
  }
}
