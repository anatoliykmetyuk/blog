package shapelesshmap

// Tests
object Alternative {
  // Poly tests
  val f = SimplePoly { x: Int     => x * x }
  val g = SimplePoly { x: String  => x * 2 }
  val m = SimplePoly { x: Double  => x / 2 }
  val k = SimplePoly { x: Boolean => !x    }
  val h = Or(Or(f, k), Or(g, m))

  println(h(2))      // 4
  println(h("Foo"))  // FooFoo
  println(h(2.2))    // 1.1
  println(h(true))   // false

  // HMap tests
  class Rel[K, V]
  trait Key { type Value }
  object Key {
    type Aux[V] = Key { type Value = V }
    implicit def rel[T, K <: Key.Aux[T]]: Rel[K, T] = new Rel[K, T]
  }

  object name extends Key { type Value = String }
  object id   extends Key { type Value = Int    }

  val map = new HMap[Rel, EmptyPoly.type](EmptyPoly)
  val newMap = map + (name, "John Smith") + (id, 10)

  println(newMap(name))  // John Smith
  println(newMap(id))    // 10

  // Natural transformations
  val natural = new ~>[List, Option] {
    def f[T](a: List[T]): Option[T] = a.headOption
  }

  println(natural(List(1)))
  println(natural(List("Foo")))

  val polyOr = Or(f, natural)
  println(polyOr(2))  
  println(polyOr(List(2)))
  println(polyOr(List("Foo")))
}


// Poly base
trait Poly

object Poly {
  implicit def polyOps[P <: Poly](p: P): PolyOps[P] = new PolyOps(p)
}

class PolyOps[P <: Poly](p: P) {
  def apply[A, B](a: A)(implicit e: Executor.Aux[P, A, B]): B = e(p, a)
}

// Executor for Poly
trait Executor[P <: Poly, A] {
  type Out
  def apply(p: P, a: A): Out
}

object Executor { type Aux[P <: Poly, A, Out0] = Executor[P, A] { type Out = Out0 } }


// Simple poly
case class SimplePoly[A, B](work: A => B) extends Poly

object SimplePoly {
  implicit def simpleExec[A, B]: Executor.Aux[SimplePoly[A, B], A, B] = new Executor[SimplePoly[A, B], A] {
    type Out = B
    def apply(p: SimplePoly[A, B], a: A): Out = p.work(a)
  }
}

// Or poly
case class Or[F <: Poly, G <: Poly](f: F, g: G) extends Poly

object Or {
  implicit def f[F <: Poly, G <: Poly, A, B](implicit e: Executor.Aux[F, A, B]): Executor.Aux[Or[F, G], A, B] = new Executor[Or[F, G], A] {
    type Out = B
    def apply(f: Or[F, G], a: A): B = e.apply(f.f, a)
  }

  implicit def g[F <: Poly, G <: Poly, A, B](implicit e: Executor.Aux[G, A, B]): Executor.Aux[Or[F, G], A, B] = new Executor[Or[F, G], A] {
    type Out = B
    def apply(f: Or[F, G], a: A): B = e.apply(f.g, a)
  }
}

// Empty poly
case object EmptyPoly extends Poly

// Natural transformations
trait ~>[F[_], G[_]] extends Poly {
  def f[T](x: F[T]): G[T]
}

object ~> {
  implicit def e[F[_], G[_], T]: Executor.Aux[~>[F, G], F[T], G[T]] = new Executor[~>[F, G], F[T]] {
    type Out = G[T]
    def apply(f: ~>[F, G], a: F[T]): G[T] = f.f(a)
  }
}

// HMap
class HMap[R[_, _], U <: Poly](val underlying: U) extends Poly {
  def +[K <: AnyRef, V](k: K, v: V)(implicit e: R[K, V]): HMap[R, Or[SimplePoly[k.type, V], U]] =
    new HMap(Or(SimplePoly[k.type, V] { _: k.type => v}, underlying))
}

object HMap {
  implicit def e[R[_, _], U <: Poly, A, B](implicit ue: Executor.Aux[U, A, B]): Executor.Aux[HMap[R, U], A, B] = new Executor[HMap[R, U], A] {
    type Out = B
    def apply(m: HMap[R, U], a: A): B = ue(m.underlying, a)
  }
}
