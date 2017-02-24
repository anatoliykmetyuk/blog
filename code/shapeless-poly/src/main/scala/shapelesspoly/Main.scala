package shapelesspoly

object AdHocPoly extends App {
  trait Case[F, In] {
    type Out
    def apply(x: In): Out
  }

  trait Poly {
    def apply[T](x: T)(implicit cse: Case[this.type, T]): cse.Out = cse(x)
  }

  object f extends Poly {
    implicit val intCase = new Case[f.type, Int] {
      type Out = String
      def apply(x: Int) = "It works! " * x
    }

    implicit val stringCase = new Case[f.type, String] {
      type Out = Int
      def apply(x: String) = x.length
    }
  }

  object g extends Poly

  println(f(3))
  println(f("Foo"))
}

object ShapelessPoly extends App {
  import shapeless._
  import poly._

  object f extends Poly1 {
    implicit val intCase    = at[Int   ] { x => "It Works! " * x}
    implicit val stringCase = at[String] { x => x.length        }
  }

  println(f(3))
  println(f("Foo"))
}