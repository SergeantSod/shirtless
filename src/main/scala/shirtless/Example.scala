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

    implicit val c1 = Conversion fromCompanion Human
    implicit val c2 = Conversion fromCompanion Monkey
    implicit val c3 = Conversion fromCompanion Banana



    val george: Human = Human("George", 12)

    println{
      val monkey: Monkey = convert(george).to[Monkey]
      monkey == Monkey("George", 12)
    }

    println{
      val hList: String ~: Int ~: Empty = convert(george).toHList
      hList == "George" ~: 12 ~: Empty
    }

  }
}
