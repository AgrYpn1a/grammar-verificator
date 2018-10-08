package object grammar {

  /*
   * CF grammar is defined with productions of
   * type:
   *
   * A -> α where α ∈ (V U T)*
   *
   */

  sealed trait Letter
  trait Var                   extends Letter { def prods: Seq[Word] }
  case class Term(s: String)  extends Letter

  implicit class Word(val letters: Seq[Letter]) {
    def apply(i: Int) = letters(i)

    def ::(that: Word): Word = new Word(this.letters ++ that.letters)
    def ::(that: Letter): Word = new Word(this.letters ++ Seq(that))

    def size = letters.size
    def map[B](f: Letter => B) = letters map f
  }

  def rule(t: Letter): Word = new Word(Seq(t))

}
