package monocle

//import monocle.Prism

/**
 * Show how could we use Optics to manipulate some Json AST
 */
class MikeExample extends MonocleSuite {

  sealed trait Json

  case class JStr(s: String) extends Json
  case class JNum(n: Int) extends Json
  case class JArray(l: List[Json]) extends Json
  case class JObj(m: Map[String, Json]) extends Json

  val jStr = Prism[Json, String]{
    case JStr(s) => Some(s)
    case _ => None}(JStr.apply)


  test("Json Prism") {
    //jStr("hello") should be ("fred")

    jStr.getOption(JStr("Hello")) should be (Some("Hello"))
  }

  test("my own traversal from example") {
    import monocle.Traversal
    import scalaz.Applicative
    import scalaz.std.map._
    import scalaz.syntax.traverse._
    import scalaz.syntax.applicative._

    def filterKey[K, V](predicate: K => Boolean): Traversal[Map[K, V], V] =
      new Traversal[Map[K, V], V] {
        def modifyF[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
          s.map {
            case (k, v) => k -> (if (predicate(k)) f(v) else v.pure[F])
          }.sequenceU
      }

    val m = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")

    val filterEven = filterKey[Int, String](key => key % 2 == 0)

    filterEven.modify(_.toUpperCase)(m) should be (Map(1 -> "one", 2 -> "TWO", 3 -> "three", 4 -> "FOUR"))
  }

  test("test Traversal modifyGetAll law") {
    import scalaz.std.list._ // to get the Traverse instance for List

    def modifyGetAllLaw[S, A](t: Traversal[S, A], s: S, f: A => A): Boolean = {
      val l = t.getAll(t.modify(f)(s))
      val r = t.getAll(s).map(f)
      println(l)
      println(r)
      l == r
    }

    val eachL = Traversal.fromTraverse[List, Int]

    modifyGetAllLaw(eachL, List(1,2,3), (x: Int) => x + 1) should be (true)
  }

  test("test Traversal composeModify law") {
    import scalaz.std.list._ // to get the Traverse instance for List
    def composedModifyLaw[S, A](t: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean = {
      val l = t.modify(g)(t.modify(f)(s))
      val r = t.modify(g compose f)(s)
      println(l)
      println(r)
      l == t
      //t.modify(g)(t.modify(f)(s)) == t.modify(g compose f)(s)
    }

    val eachL = Traversal.fromTraverse[List, Int]

    composedModifyLaw(eachL, List(1,2,3), (x: Int) => x + 1, (y: Int) => y + 2) == (true)

  }
}

