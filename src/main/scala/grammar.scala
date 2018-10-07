import collection._

object Main {
  sealed trait Letter
  sealed trait Var  extends Letter { def prods: Seq[Word] }
  sealed trait Term extends Letter


  /*
   * CF grammar is defined with productions of
   * type:
   *
   * A -> α where α ∈ (V U T)*
   *
   */
  type Word = mutable.Seq[Letter]

  // We define concrete grammar here

  // Non-empty grammar
  case class S() extends Var {
    override def prods = mutable.Seq[Word](
      mutable.Seq(Empty()), 
      mutable.Seq(A(), S(), B()))
  }
  
  // Empty grammar
  case class S2() extends Var {
    override def prods = mutable.Seq[Word]( 
      mutable.Seq(S2()))
  }

  // Terminals
  case class Empty()  extends Term { override def toString = "" }
  case class A()      extends Term { override def toString = "a" }
  case class B()      extends Term { override def toString = "b" }

  var w: Word = mutable.Seq()

  def backtrack(curr: Letter, depth: Int, n: Int): Unit = curr match {
    case t: Term          => w = w :+ t; Unit
    case v: Var if depth < n  => 
      for(i: Int <- 0 until v.prods.size)
        for(j: Int <- 0 until v.prods(i).size)
          backtrack(v.prods(i)(j), depth + 1, n)
    case v: Var if depth >= n => Unit
  }

  class PrettyPrinter {
    def apply(w: Word): String = if (w.size == 0) "ø" else w.toSeq.map(x => x match {
      case Empty() => ""
      case t: Term => t.toString
      case t: Var=> t.toString
      case _ => ""
    }).foldLeft("")((a, b) => a + b)
  }

  def main(args: Array[String]): Unit = {
    val p = new PrettyPrinter()
    backtrack(S(), 0, 1)
    println(p(w))
  }
}
