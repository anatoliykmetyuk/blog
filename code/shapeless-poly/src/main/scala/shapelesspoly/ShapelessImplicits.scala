package shapelesspoly

import cats.Monoid
import cats.instances.list._
import cats.syntax.all._

import shapeless._
import shapeless.poly._
import shapeless.ops.hlist._

trait FragmentContainer {
  // Route framework
  trait Fragment[T] { val chunk: T }
  case class StringFrag(chunk: String) extends Fragment[String]
  case class IntFrag   (chunk: Int   ) extends Fragment[Int   ]

  val route: Fragment[String] :: Fragment[Int] :: HNil =
    StringFrag("foo") :: IntFrag(1) :: HNil
}

trait ImplicitsContainer { this: FragmentContainer =>
  // Components to use fold() to construct a URL
  object toUrl extends ~>>[Fragment, String] {
    override def apply[T](frag: Fragment[T]): String = frag.chunk.toString
  }

  implicit val strMonoid: Monoid[String] = new Monoid[String] {
    override val empty = ""
    override def combine(a: String, b: String) = s"$a/$b"
  }
}

object Fold1 extends FragmentContainer with ImplicitsContainer with App {
  // Need to define URL, something like fold(route, x => x.chunk.toString)
  def fold[
      ROUTE <: HList
    , H, HR: Monoid
    , T <: HList, TR <: HList
  ](r: ROUTE, f: Fragment ~>> HR)(implicit
      hc  : Case1 .Aux[f.type, H, HR]
    , mt  : Mapper.Aux[f.type, ROUTE, HR :: TR]
    , trav: ToTraversable.Aux[HR :: TR, List, HR]
  ): HR =
    r.map(f).toList[HR].combineAll

  // Usage
  val url = fold[
    Fragment[String] :: Fragment[Int] :: HNil
  , Fragment[String], String
  , Fragment[Int] :: HNil, String :: HNil
  ](route, toUrl)

  println(url)
}

object Fold2 extends FragmentContainer with ImplicitsContainer with App {
  def fold[
      ROUTE <: HList
    , H, HR: Monoid
    , T <: HList, TR <: HList
    , TLen <: Nat
  ](r: ROUTE, f: Fragment ~>> HR)(implicit
      cons: IsHCons.Aux[ROUTE, H, T]
    , tlen: Length .Aux[T, TLen]
    , tr  : Fill   .Aux[TLen, HR, TR]

    , hc  : Case1 .Aux[f.type, H, HR]
    , mt  : Mapper.Aux[f.type, ROUTE, HR :: TR]
    , trav: ToTraversable.Aux[HR :: TR, List, HR]
  ): HR =
    r.map(f).toList[HR].combineAll

  def url[
      ROUTE <: HList
    , H
    , T <: HList, TR <: HList
    , TLen <: Nat
  ](r: ROUTE)(implicit
      cons: IsHCons.Aux[ROUTE, H, T]
    , tlen: Length .Aux[T, TLen]
    , tr  : Fill   .Aux[TLen, String, TR]

    , hc  : Case1 .Aux[toUrl.type, H, String]
    , mt  : Mapper.Aux[toUrl.type, ROUTE, String :: TR]
    , trav: ToTraversable.Aux[String :: TR, List, String]
  ): String =
    fold(r, toUrl)

  val urlStr = url(route)
  println(urlStr)
}

trait FoldBundleContainer {
  trait FoldBundle[ROUTE <: HList, HR, F] {
    // HR: Monoid
    implicit val mon: Monoid[HR]

    // IsHCons.Aux[ROUTE, H, T]
    implicit val cons: IsHCons[ROUTE]
    type H = cons.H
    type T = cons.T

    // Length.Aux[T, TLen]
    implicit val tlen: Length[T]
    type TLen = tlen.Out

    // Fill.Aux[TLen, HR, TR]
    implicit val tr: Fill[TLen, HR]
    type TR = tr.Out

    // Literaly
    implicit val hc  : Case1.Aux[F, H, HR]
    implicit val mt  : Mapper.Aux[F, ROUTE, HR :: TR]
    implicit val trav: ToTraversable.Aux[HR :: TR, List, HR]    
  }

  object FoldBundle {
    implicit def instance[
        ROUTE <: HList
      , F
      , H, HR
      , T <: HList, TR <: HList
      , TLen <: Nat
    ](implicit
        _mon : Monoid[HR]
      , _cons: IsHCons.Aux[ROUTE, H, T]
      , _tlen: Length .Aux[T, TLen]
      , _tr  : Fill   .Aux[TLen, HR, TR]

      , _hc  : Case1 .Aux[F, H, HR]
      , _mt  : Mapper.Aux[F, ROUTE, HR :: TR]
      , _trav: ToTraversable.Aux[HR :: TR, List, HR]
    ): FoldBundle[ROUTE, HR, F] = new FoldBundle[ROUTE, HR, F] {
      override implicit val mon  = _mon
      override implicit val cons = _cons
      override implicit val tlen = _tlen
      override implicit val tr   = _tr
      override implicit val hc   = _hc
      override implicit val mt   = _mt
      override implicit val trav = _trav
    }
  }
}

object Fold3 extends FragmentContainer with ImplicitsContainer with FoldBundleContainer with App {
  def fold[ROUTE <: HList, HR](r: ROUTE, f: Fragment ~>> HR)(implicit fb: FoldBundle[ROUTE, HR, f.type]): HR = {
    import fb._
    r.map(f).toList[HR].combineAll
  }

  def url[ROUTE <: HList](r: ROUTE)(implicit fb: FoldBundle[ROUTE, String, toUrl.type]): String =
    fold(r, toUrl)

  val urlStr = url(route)
  println(urlStr)
}

object Fold4 extends FragmentContainer with FoldBundleContainer with App {
  case class Route[ROUTE <: HList](path: ROUTE) {
    def fold[HR](f: Fragment ~>> HR)(implicit fb: FoldBundle[ROUTE, HR, f.type]): HR = {
      import fb._
      path.map(f).toList[HR].combineAll
    }

    object toUrl extends ~>>[Fragment, String] {
      override def apply[T](frag: Fragment[T]): String = frag.chunk.toString
    }

    implicit val strMonoid: Monoid[String] = new Monoid[String] {
      override val empty = ""
      override def combine(a: String, b: String) = s"$a/$b"
    }

    def url(implicit fb: FoldBundle[ROUTE, String, toUrl.type]) = fold(toUrl)
  }

  val r = Route(route)
  import r.strMonoid
  println(r.url)
}

object Fold5 extends FragmentContainer with ImplicitsContainer with App {

  object folder extends Poly2 {
    implicit val strStr = at[String, Fragment[String]] { (s1, s2) => s"$s1/${s2.chunk}" }
    implicit val strInt = at[String, Fragment[Int   ]] { (s1, i2) => s"$s1/${i2.chunk}" }
  }

  val url = route.foldLeft("")(folder)
  println(url)
}