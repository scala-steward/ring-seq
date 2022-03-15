import RingSeq._

import org.scalatest.flatspec._
import org.scalatest.matchers._

class ScaladocExampleSpec extends AnyFlatSpec with should.Matchers {

  "The Seq examples in Scaladoc" must "be correct" in {
    Seq(0, 1, 2).applyO(3) shouldBe 0
    Seq(0, 1, 2).rotateRight(1) shouldBe Seq(2, 0, 1)
    Seq(0, 1, 2).rotateLeft(1) shouldBe Seq(1, 2, 0)
    Seq(0, 1, 2).startAt(1) shouldBe Seq(1, 2, 0)
    Seq(0, 1, 2).reflectAt() shouldBe Seq(0, 2, 1)
    Seq(0, 1, 2).segmentLengthO(_ % 2 == 0, 2) shouldBe 2
    Seq(0, 1, 2).sliceO(-1, 4) shouldBe Seq(2, 0, 1, 2, 0)
    Seq(0, 1, 2).containsSliceO(Seq(2, 0, 1, 2, 0)) shouldBe true
    Seq(0, 1, 2).indexOfSliceO(Seq(2, 0, 1, 2, 0)) shouldBe 2
    Seq(0, 1, 2, 0, 1, 2).lastIndexOfSliceO(Seq(2, 0)) shouldBe 5
    Seq(0, 1, 2).slidingO(2).toList shouldBe List(Seq(0, 1), Seq(1, 2), Seq(2, 0))
  }

  "The String examples in Scaladoc" must "be correct" in {
    "ABC".applyO(3) shouldBe 'A'
    "ABC".rotateRight(1) shouldBe "CAB"
    "ABC".rotateLeft(1) shouldBe "BCA"
    "ABC".startAt(1) shouldBe "BCA"
    "ABC".reflectAt() shouldBe "ACB"
    "ABA".segmentLengthO(_ == 'A', 2) shouldBe 2
    "ABC".sliceO(-1, 4) shouldBe "CABCA"
    "ABC".containsSliceO("CABCA") shouldBe true
    "ABC".indexOfSliceO("CABCA") shouldBe 2
    "ABCABC".lastIndexOfSliceO("CA") shouldBe 5
    "ABC".slidingO(2).toList shouldBe List("AB", "BC", "CA")
  }

}
