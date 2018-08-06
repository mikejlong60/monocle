package monocle

import monocle.function.Plated

/**
 * Show how could we use Optics to manipulate some Json AST
 */
class JsonExample extends MonocleSuite {

  sealed trait Json

  case class JsString(s: String) extends Json
  case class JsNumber(n: Int) extends Json
  case class JsArray(l: List[Json]) extends Json
  case class JsObject(m: Map[String, Json]) extends Json

  val jsString = Prism[Json, String] { case JsString(s) => Some(s); case _ => None }(JsString.apply)
  val jsNumber = Prism[Json, Int] { case JsNumber(n) => Some(n); case _ => None }(JsNumber.apply)
  val jsArray = Prism[Json, List[Json]] { case JsArray(a) => Some(a); case _ => None }(JsArray.apply)
  val jsObject = Prism[Json, Map[String, Json]] { case JsObject(m) => Some(m); case _ => None }(JsObject.apply)

  val json: Json = JsObject(Map(
    "first_name" -> JsString("John"),
    "last_name" -> JsString("Doe"),
    "age" -> JsNumber(28),
    "siblings" -> JsArray(List(
      JsObject(Map(
        "first_name" -> JsString("Elia"),
        "age" -> JsNumber(23)
      )),
      JsObject(Map(
        "first_name" -> JsString("Robert"),
        "age" -> JsNumber(25)
      ))
    ))
  ))

  test("Json Prism") {
    jsNumber.getOption(JsString("plop")) shouldEqual None
    jsNumber.getOption(JsNumber(2)) shouldEqual Some(2)
  }

  test("Compose Lens with a Traversal, etc. to reach down deep and change something.") {
    import monocle.Traversal
    import scalaz.std.list._ // to get the Traverse instance for List
    case class Town(residents: List[Resident])
    case class Resident(firstName: String, lastName: String, address: Address)
    case class Address(strNumber: Int, streetName: String, phones: List[Phone])
    case class Phone(areaCode: Int, firstThree: Int, lastFour: Int)

    val _resident = Lens[Town, List[Resident]](whole => whole.residents)(part => whole => whole.copy(residents = part))
    val _residentT = Traversal.fromTraverse[List, Resident]
    val _address = Lens[Resident, Address](whole => whole.address)(part => whole => whole.copy(address = part))
    val _phones = Lens[Address, List[Phone]](whole => whole.phones)(part => whole => whole.copy(phones = part))
    val _phonesT = Traversal.fromTraverse[List, Phone]

    val trav = _resident composeTraversal _residentT composeLens _address composeLens _phones composeTraversal _phonesT

    val changeAreaCodeIf540 = (phone: Phone) => phone match { //Notice that this is a total function
      case phone if phone.areaCode == 540 => Phone(areaCode = 443, firstThree = phone.firstThree, lastFour = phone.lastFour)
      case phone if phone.areaCode == 757 => Phone(areaCode = 804, firstThree = phone.firstThree, lastFour = phone.lastFour)
      case _ => phone
    }

    val winchester = Town(residents = List(Resident("mike", "long", Address(103, "Leavenworth Court", phones = List(
      Phone(333, 444, 5555),
      Phone(757, 259, 9559),
      Phone(540, 678, 1249),
      Phone(540, 336, 9118),
      Phone(540, 431, 1253)
    )))))

    val wincWithNewPhones = trav.modify(changeAreaCodeIf540)(winchester)
    wincWithNewPhones shouldBe (Town(residents = List(Resident("mike", "long", Address(103, "Leavenworth Court", phones = List(
      Phone(333, 444, 5555),
      Phone(804, 259, 9559),
      Phone(443, 678, 1249),
      Phone(443, 336, 9118),
      Phone(443, 431, 1253)
    ))))))
  }

  test("Compose Lens with a Traversal, etc. to reach down deep and change something and change its type.") {
    import monocle.Traversal
    import scalaz.std.list._ // to get the Traverse instance for List
    case class Town(residents: List[Resident])
    case class Resident(firstName: String, lastName: String, address: Address)
    case class Address(strNumber: Int, streetName: String, phones: List[Phone])
    sealed trait Phone
    case class PhoneNum(phone: Long) extends Phone
    case class PhoneStr(phone: String) extends Phone

    case class AddressStr(strNumber: Int, streetName: String, phones: List[PhoneStr])

    val _resident = Lens[Town, List[Resident]](whole => whole.residents)(part => whole => whole.copy(residents = part))
    val _residentT = Traversal.fromTraverse[List, Resident]
    val _address = Lens[Resident, Address](whole => whole.address)(part => whole => whole.copy(address = part))
    val _phones = Lens[Address, List[Phone]](whole => whole.phones)(part => whole => whole.copy(phones = part))
    val _phonesT = Traversal.fromTraverse[List, Phone]

    val trav = _resident composeTraversal _residentT composeLens _address composeLens _phones composeTraversal _phonesT

    val changeAreaCodeIf540 = (phone: PhoneNum) => phone match { //Notice that this is a total function
      case phone if phone.phone / 10000000 == 540 => PhoneNum(5401111111l)
      case phone if phone.phone / 10000000 == 757 => PhoneNum(8041111111l)
      case _ => phone
    }

    val winchester = Town(residents = List(Resident("mike", "long", Address(103, "Leavenworth Court", phones = List(
      PhoneNum(3334445555l),
      PhoneNum(7572599559l),
      PhoneNum(5406781249l),
      PhoneNum(5403369118l),
      PhoneNum(5404311253l),
      PhoneStr("333-444-5555"),
      PhoneStr("757-259-9559"),
      PhoneStr("540-678-1249"),
      PhoneStr("540-336-9118"),
      PhoneStr("540-431-1253")
    )))))

    //import monocle.Iso
    //    val personToTuple = Iso[Person, (String, Int)](p => (p.name, p.age)) { case (name, age) => Person(name, age) }
    //val phoneToStrPhone = Iso[PhoneNum, PhoneStr](p => PhoneStr(s"${p.areaCode} - ${p.firstThree} - ${p.lastFour}")) {
    //  case PhoneStr(p) => PhoneNum(areaCode = p.substring(0, 3).toInt, firstThree = p.substring(4, 3).toInt, lastFour = p.substring(8, 4).toInt)
    // }

    //val jsString = Prism[Json, String] { case JsString(s) => Some(s); case _ => None }(JsString.apply)
    //val jsNumber = Prism[Json, Int] { case JsNumber(n) => Some(n); case _ => None }(JsNumber.apply)

    val phoneStr1 = Prism[Phone, String] {
      case PhoneStr(phone) if phone.substring(0, 3) == "540" => Some(s"443-${phone.substring(4)}") //=> Some(phone)
      case PhoneStr(phone) if phone.substring(0, 3) == "757" => Some(s"804-${phone.substring(4)}") //=> Some(phone)
      case _ => {
        println("phoneStr")
        None
      }
    }(PhoneStr.apply)

    val phoneNum1 = Prism[Phone, Long] {
      case PhoneNum(phone) if phone.toString.substring(0, 3) == "540" => Some(s"443${phone.toString.substring(4)}".toLong) //=> Some(phone)
      case PhoneNum(phone) if phone.toString.substring(0, 3) == "757" => Some(s"804${phone.toString.substring(4)}".toLong) //=> Some(phone)
      //case PhoneNum(number) => Some(number)
      case _ => {
        println("phoneNum")
        None
      }
    }(PhoneNum.apply)

    val trav2 = _resident composeTraversal _residentT composeLens _address composeLens _phones composeTraversal _phonesT //composePrism phoneNum1 //Int1 //composePrism phoneStr1// phoneToStrPhone

    val changeAreaCodeIf540Num = (phone: Long) => phone match { //Notice that this is a total function
      case phone if phone / 10000000 == 540 => 4431111111l
      case phone if phone / 10000000 == 757 => 8041111111l
      case _ => phone
    }

    val changeAreaCodeIf540Str = (phone: Phone) => phone match { //Notice that this is a total function
      case PhoneStr(phone) if phone.substring(0, 3) == "540" => PhoneStr(s"443-${phone.substring(4)}")
      case PhoneStr(phone) if phone.substring(0, 3) == "757" => PhoneStr(s"804-${phone.substring(4)}")
      case PhoneNum(phone) if phone.toString.substring(0, 3) == "540" => PhoneNum(s"443${phone.toString.substring(3)}".toLong) //=> Some(phone)
      case PhoneNum(phone) if phone.toString.substring(0, 3) == "757" => PhoneNum(s"804${phone.toString.substring(3)}".toLong)//=> Some(phone)
      case x:Phone => x
    }

    val wincWithNewPhones = trav2.modify(changeAreaCodeIf540Str)(winchester)
    wincWithNewPhones shouldBe (Town(residents = List(Resident("mike", "long", Address(103, "Leavenworth Court", phones = List(
      PhoneNum(3334445555l),
      PhoneNum(8042599559l),
      PhoneNum(4436781249l),
      PhoneNum(4433369118l),
      PhoneNum(4434311253l),
      PhoneStr("333-444-5555"),
      PhoneStr("804-259-9559"),
      PhoneStr("443-678-1249"),
      PhoneStr("443-336-9118"),
      PhoneStr("443-431-1253")
    ))))))

  }

  test("Use index to go into an JsObject or JsArray") {
    (jsObject composeOptional index("age") composePrism jsNumber).getOption(json) shouldEqual Some(28)

    (jsObject composeOptional index("siblings")
      composePrism jsArray
      composeOptional index(1)
      composePrism jsObject
      composeOptional index("first_name")
      composePrism jsString).set("Robert Jr.")(json) shouldEqual JsObject(Map(
        "first_name" -> JsString("John"),
        "last_name" -> JsString("Doe"),
        "age" -> JsNumber(28),
        "siblings" -> JsArray(List(
          JsObject(Map(
            "first_name" -> JsString("Elia"),
            "age" -> JsNumber(23)
          )),
          JsObject(Map(
            "first_name" -> JsString("Robert Jr."), // name is updated
            "age" -> JsNumber(25)
          ))
        ))
      ))
  }

  test("Use at to add delete fields") {
    (jsObject composeLens at("nick_name")).set(Some(JsString("Jojo")))(json) shouldEqual JsObject(Map(
      "first_name" -> JsString("John"),
      "nick_name" -> JsString("Jojo"), // new field
      "last_name" -> JsString("Doe"),
      "age" -> JsNumber(28),
      "siblings" -> JsArray(List(
        JsObject(Map(
          "first_name" -> JsString("Elia"),
          "age" -> JsNumber(23)
        )),
        JsObject(Map(
          "first_name" -> JsString("Robert"),
          "age" -> JsNumber(25)
        ))
      ))
    ))

    (jsObject composeLens at("age")).set(None)(json) shouldEqual JsObject(Map(
      "first_name" -> JsString("John"),
      "last_name" -> JsString("Doe"), // John is ageless now
      "siblings" -> JsArray(List(
        JsObject(Map(
          "first_name" -> JsString("Elia"),
          "age" -> JsNumber(23)
        )),
        JsObject(Map(
          "first_name" -> JsString("Robert"),
          "age" -> JsNumber(25)
        ))
      ))
    ))
  }

  test("Use each and filterIndex to modify several fields at a time") {
    (jsObject composeTraversal filterIndex((_: String).contains("name"))
      composePrism jsString
      composeOptional headOption).modify(_.toLower)(json) shouldEqual JsObject(Map(
        "first_name" -> JsString("john"), // starts with lower case
        "last_name" -> JsString("doe"), // starts with lower case
        "age" -> JsNumber(28),
        "siblings" -> JsArray(List(
          JsObject(Map(
            "first_name" -> JsString("Elia"),
            "age" -> JsNumber(23)
          )),
          JsObject(Map(
            "first_name" -> JsString("Robert"),
            "age" -> JsNumber(25)
          ))
        ))
      ))

    (jsObject composeOptional index("siblings")
      composePrism jsArray
      composeTraversal each
      composePrism jsObject
      composeOptional index("age")
      composePrism jsNumber).modify(_ + 1)(json) shouldEqual JsObject(Map(
        "first_name" -> JsString("John"),
        "last_name" -> JsString("Doe"),
        "age" -> JsNumber(28),
        "siblings" -> JsArray(List(
          JsObject(Map(
            "first_name" -> JsString("Elia"),
            "age" -> JsNumber(24) // Elia is older
          )),
          JsObject(Map(
            "first_name" -> JsString("Robert"),
            "age" -> JsNumber(26) // Robert is older
          ))
        ))
      ))
  }

  implicit val jsonPlated: Plated[Json] = new Plated[Json] {
    import scalaz.Applicative
    import scalaz.std.list._
    import scalaz.std.map._
    import scalaz.syntax.traverse._

    val plate: Traversal[Json, Json] = new Traversal[Json, Json] {
      def modifyF[F[_]: Applicative](f: Json => F[Json])(a: Json): F[Json] =
        a match {
          case j @ (JsString(_) | JsNumber(_)) => Applicative[F].pure(j)
          case JsArray(l) => l.traverse(f).map(JsArray)
          case JsObject(m) => m.traverse(f).map(JsObject)
        }
    }
  }

  test("Plated instance to rewrite any matching elements") {
    Plated.rewrite[Json] {
      case JsString(s) =>
        val u = s.toUpperCase
        if (s != u) Some(JsString(u)) else None
      case _ => None
    }(json) shouldEqual JsObject(Map(
      "first_name" -> JsString("JOHN"),
      "last_name" -> JsString("DOE"),
      "age" -> JsNumber(28),
      "siblings" -> JsArray(List(
        JsObject(Map(
          "first_name" -> JsString("ELIA"),
          "age" -> JsNumber(23)
        )),
        JsObject(Map(
          "first_name" -> JsString("ROBERT"),
          "age" -> JsNumber(25)
        ))
      ))
    ))
  }
}
