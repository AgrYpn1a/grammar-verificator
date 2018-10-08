import collection._
import grammar._

object Main {

  // We define concrete grammar here
  // by start symbol Si and productions

  // Non-empty grammar
  case object S extends Var {
    override def prods = Seq[Word](
      Seq(empty), 
      Seq(a, S, b))
  }
  
  // Empty grammar
  case object S2 extends Var {
    override def prods = Seq[Word]( 
      Seq(S2))
  }

  // Terminals
  val empty = Term("λ")
  val a = Term("a")
  val b = Term("b")

  var w: Word = new Word(Seq())

  def backtrack(curr: Letter, depth: Int, n: Int): Unit = curr match {
    // TODO should rather be w :: t, having t :: w in that context
    // is also nice
    case t: Term              => w = t :: w; Unit
    case v: Var if depth < n  => 
      for(i: Int <- 0 until v.prods.size)
        for(j: Int <- 0 until v.prods(i).size)
          backtrack(v.prods(i)(j), depth + 1, n)
    case v: Var if depth >= n => Unit
  }

  class PrettyPrinter {
    def apply(w: Word): String = if (w.size == 0) "ø" else w.map(x => x match {
      case t: Term 
        if t.s == "λ" && w.size == 1  => "λ"
      case t: Term if t.s == "λ"       => ""
      case t: Term                    => t.s
      case t: Var                     => t.toString
      case _                          => ""
    }).foldLeft("")((a, b) => a + b)
  }

  def main(args: Array[String]): Unit = {
    val p = new PrettyPrinter()
    backtrack(S, 0, 3)
    println(p(w))
  }
}
