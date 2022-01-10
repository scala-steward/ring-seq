import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import math.Ordering.Implicits.seqOrdering
import scala.collection.immutable.Queue

class RotationsReflectionsSpec extends AnyFlatSpec with RingSeq with should.Matchers {

  val s = Seq(1, 2, 3, 4, 5)
  val oneLeft = Seq(2, 3, 4, 5, 1)

  "Any Seq subtype" can "be rotated" in {
    "SCALA".rotateRight(2).mkString shouldEqual "LASCA"
    s.toList.rotateRight(2) shouldBe List(4, 5, 1, 2, 3)
    s.toVector.rotateRight(2) shouldBe Vector(4, 5, 1, 2, 3)
    Queue(1, 2, 3, 4, 5).rotateRight(2) shouldBe Queue(4, 5, 1, 2, 3)
  }

  "A Seq considered as a ring" can "be rotated one step to the right" in {
    s.rotateRight(1) shouldBe Seq(5, 1, 2, 3, 4)
  }

  it can "be rotated one step to the left" in {
    s.rotateLeft(1) shouldBe oneLeft
    s.rotateRight(-1) shouldBe oneLeft
  }

  it can "be rotated to start where index 1 is" in {
    s.startAt(1) shouldBe oneLeft
  }

  it can "be reflected" in {
    s.reflectAt() shouldBe Seq(1, 5, 4, 3, 2)
  }

  it can "be reflected at a given index" in {
    s.reflectAt(2) shouldBe Seq(3, 2, 1, 5, 4)
  }

  it can "iterate on all rotations" in {
    s.allRotations.toList shouldBe List(
      s,
      oneLeft,
      Seq(3, 4, 5, 1, 2),
      Seq(4, 5, 1, 2, 3),
      Seq(5, 1, 2, 3, 4)
    )
  }

  it can "iterate on all rotations and reflections" in {
    s.allRotationsAndReflections.toList shouldBe List(
      s,
      oneLeft,
      Seq(3, 4, 5, 1, 2),
      Seq(4, 5, 1, 2, 3),
      Seq(5, 1, 2, 3, 4),
      Seq(5, 4, 3, 2, 1),
      Seq(4, 3, 2, 1, 5),
      Seq(3, 2, 1, 5, 4),
      Seq(2, 1, 5, 4, 3),
      Seq(1, 5, 4, 3, 2)
    )
  }

  it can "return the sorted minimum rotation" in {
    Seq(1, 2, 3, 4, 1).minRotation shouldBe Seq(1, 1, 2, 3, 4)
  }

  it can "be the rotation of another Seq" in {
    Seq.empty.isRotationOf(Seq.empty) shouldBe true
    s.isRotationOf(s) shouldBe true
    s.isRotationOf(Seq(3, 4, 5, 1, 2)) shouldBe true
    s.allRotations.forall(s.isRotationOf) shouldBe true
  }

  it can "be the reflection of another Seq" in {
    s.isReflectionOf(s) shouldBe true
    s.isReflectionOf(Seq(1, 5, 4, 3, 2)) shouldBe true
  }

  it can "be the rotation or reflection of another Seq" in {
    Seq.empty.isRotationOrReflectionOf(Seq.empty) shouldBe true
    s.isRotationOrReflectionOf(s) shouldBe true
    s.isRotationOrReflectionOf(Seq(3, 2, 1, 5, 4)) shouldBe true
    s.allRotationsAndReflections.forall(s.isRotationOrReflectionOf) shouldBe true
  }
}
