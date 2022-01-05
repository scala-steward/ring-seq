import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.check
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class RotationsReflectionsSpec extends AnyFlatSpec with RingVector with should.Matchers {

  val v = Vector(1, 2, 3, 4, 5)

  "A Vector considered as a ring" can "be rotated one step to the right" in {
    v.rotateRight(1) shouldBe Vector(5, 1, 2, 3, 4)
  }

  it can "be rotated one step to the left, to start where index 1 was" in {
    v.startAt(1) shouldBe Vector(2, 3, 4, 5, 1)
  }

  it can "be reflected" in {
    v.reflectAt() shouldBe Vector(1, 5, 4, 3, 2)
  }

  it can "be reflected at a given index" in {
    v.reflectAt(2) shouldBe Vector(3, 2, 1, 5, 4)
  }

  it can "iterate on all rotations" in {
    v.allRotations.toList shouldBe List(
      v,
      Vector(2, 3, 4, 5, 1),
      Vector(3, 4, 5, 1, 2),
      Vector(4, 5, 1, 2, 3),
      Vector(5, 1, 2, 3, 4)
    )
  }

  it can "return the sorted minimum rotation" in {
    val w = Vector(1, 2, 3, 4, 1)
    w.minRotation shouldBe Vector(1, 1, 2, 3, 4)
  }

  it can "be the rotation of another Vector" in {
    v.isRotationOf(Vector(3, 4, 5, 1, 2)) shouldBe true
  }

  it can "be the reflection of another Vector" in {
    v.isReflectionOf(Vector(1, 5, 4, 3, 2)) shouldBe true
  }

  it can "be the rotation or reflection of another Vector" in {
    v.isRotationOrReflectionOf(Vector(3, 2, 1, 5, 4)) shouldBe true
  }
}