package shirtless

import shirtless.hlist._

object Example {

  def convert[T](convertee: T)(implicit tAsR: Conversion[T]) = new {
    def to[U](implicit uAsR: U As tAsR.Representation):U = {
      uAsR from (tAsR to convertee)
    }

    def toHList:tAsR.Representation = tAsR to convertee
  }

  def main(args:Array[String]):Unit = {

    val someHList = 12.0 ~: "String" ~: 12 ~: Empty

    case class Banana(weight: Double)
    case class Monkey(name: String, age: Int)
    case class Human(name: String, age: Int)
    case class GenericCaseClass[T](name: T, age: Int)

    //Explicit opt-in is required to define implicits (excuse the pun)
    implicit val c1 = Conversion fromCompanion Human
    implicit val c2 = Conversion fromCompanion Monkey
    implicit val c3 = Conversion fromCompanion Banana
    implicit def c4[T] = Conversion.fromCompanion(GenericCaseClass[T](_,_))


    val george: Human = Human("George", 12)

    val asHList: String ~: Int ~: Empty = convert(george).toHList
    println(asHList)
    println(asHList == "George" ~: 12 ~: Empty)

    val asMonkey: Monkey = convert(george).to[Monkey]
    println(asMonkey)
    println(asMonkey == Monkey("George", 12))

    val asGeneric: GenericCaseClass[String] = convert(george).to[GenericCaseClass[String]]
    println(asGeneric)
    println(asGeneric == GenericCaseClass("George", 12))


  }
}
