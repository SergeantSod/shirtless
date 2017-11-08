import scala.annotation.implicitNotFound

import shirtless.hlist._

package object shirtless {

  // Corresponds to Gen
  @implicitNotFound("No known way to convert ${T} to an HList representation.")
  trait Conversion[T]{
    type Representation <: HList
    def to(value: T): Representation
    def from(representation: Representation): T
  }

  class ProductConversion[T <: Product, R <: HList, Tuple](builder: Tuple => T, representationMatcher: PartialFunction[R,Tuple]) extends Conversion[T]{
    override type Representation = R

    override def to(value: T): Representation = unsafeFromProduct(value)
    def from(representation: Representation): T = {
      builder(representationMatcher(representation))
    }

    private def unsafeFromProduct(product:Product):R = product.productIterator.foldRight(Empty:HList){ (l, r) => new ~:(l,r) }.asInstanceOf[R]
  }

  object ProductConversion{
    def apply[T <: Product, R <: HList, Tuple](builder: Tuple => T)(representationMatcher: PartialFunction[R,Tuple]) = new ProductConversion(builder, representationMatcher)
  }

  // Corresponds to Gen.Aux
  @implicitNotFound("No known way to convert ${T} to ${R}")
  type As[T,R] = Conversion[T]{ type Representation = R }

  object Conversion{

    // Factory methods for Conversion instances.
    // This only works because Companion objects inherit from Function
    // and we can overload and let type inference take care of the rest.
    def fromCompanion[A, P <: Product](companion: A => P): P As (A ~: Empty) = ProductConversion(companion){case a ~: Empty => a}
    def fromCompanion[A, B, P <: Product](companion: (A,B) => P): P As (A ~: B ~: Empty) = ProductConversion(companion.tupled){case a ~: b ~: Empty => (a,b)}
    def fromCompanion[A, B, C, P <: Product](companion: (A,B,C) => P): P As (A ~: B ~: C ~: Empty) = ProductConversion(companion.tupled){case a ~: b ~: c ~: Empty => (a,b,c)}

    // Imagine more arities here.
  }

}
