import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.check
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class RingVectorSpec extends AnyFlatSpec with RingVector with should.Matchers {

  val v = Vector(1, 2, 3, 4, 5)

  "A Vector considered as a ring" must "always have an element before another one" in {
    v.applyO(-1) shouldBe 5
  }

  it must "always have an element after another one" in {
    v.applyO(5) shouldBe 1
  }

  it must "return an element for any index" in {
    check(forAll(Gen.choose(-1000, 1000))(
      i => v.contains(v.applyO(i))
    ))
  }

  "A RingVector" can "be rotated one step to the right" in {
    v.rotateRight(1) shouldBe Vector(5, 1, 2, 3, 4)
  }

  it can "be rotated one step to the left, now starts where index 1 was" in {
    v.startAt(1) shouldBe Vector(2, 3, 4, 5, 1)
  }

  it can "be reflected" in {
    v.reflectAt() shouldBe Vector(1, 5, 4, 3, 2)
  }

  it can "be sliced to a circular slice" in {
    v.sliceO(-1, 6) shouldBe Vector(5, 1, 2, 3, 4, 5, 1)
  }

  it can "contain a circular slice" in {
    v.containsSliceO(Vector(5, 1)) shouldBe true
  }

  it can "return the index of a contained circular slice" in {
    v.indexOfSliceO(Vector(4, 5, 1)) shouldBe 3
  }
  
  it can "be ordinarily slided" in {
    v.sliding(2).toList shouldBe List(
      Vector(1, 2),
      Vector(2, 3),
      Vector(3, 4),
      Vector(4, 5)
    )
    v.sliding(2, 2).toList shouldBe List(
      Vector(1, 2),
      Vector(3, 4)
    )
  }

  it can "be slided to circular windows" in {
    v.slidingO(2).toList shouldBe List(
      Vector(1, 2),
      Vector(2, 3),
      Vector(3, 4),
      Vector(4, 5),
      Vector(5, 1)
    )
    v.slidingO(2, 2).toList shouldBe List(
      Vector(1, 2),
      Vector(3, 4),
      Vector(5, 1),
      Vector(2, 3),
      Vector(4, 5)
    )
  }

}
