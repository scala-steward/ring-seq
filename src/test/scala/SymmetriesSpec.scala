import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.check
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class SymmetriesSpec extends AnyFlatSpec with RingVector with should.Matchers {

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

}