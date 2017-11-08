package shirtless

package object hlist {
  sealed trait HList{}

  trait ConsesTo[Tail <: HList]{ self : Tail =>
    def ~:[T](newHead: T): (T ~: Tail) = new ~:(newHead, this)
  }

  sealed trait Empty extends HList with ConsesTo[Empty] {
    override def toString = "Empty"
  }

  val Empty = new Empty{}

  final case class ~:[Head, Tail <: HList](head: Head, tail: Tail) extends HList with ConsesTo[Head ~: Tail]{
    override def toString = head.toString + " <-: " + tail.toString
  }
}
