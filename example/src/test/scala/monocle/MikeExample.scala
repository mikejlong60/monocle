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
    val actual = jStr("hello")
    println(actual)
    actual should be ("fred")

    val actual2 = jStr.getOption(JStr("Hello"))
    println(actual2)
  }

  test("traversal fancy") {
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

    filterEven.modify(_.toUpperCase)(m) should be ("fred")


  }
}

