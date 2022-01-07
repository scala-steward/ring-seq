import scala.collection.immutable.SeqOps
import scala.Ordering.Implicits.*

trait RingSeq:
  
  /* for improved readability, a Seq index */
  type Index = Int

  /* and a RingSeq index, any value is valid */
  type IndexO = Int

  extension[A, B[_] <: SeqOps[A, B, B[A]]](ring: B[A])

    private def index(i: IndexO): Index =
      java.lang.Math.floorMod(i, ring.size)

    def applyO(i: IndexO): A =
      ring(index(i))

    def rotateRight(step: Int): B[A] =
      if ring.isEmpty then ring
      else
        val j: Index = ring.size - index(step)
        ring.drop(j) ++ ring.take(j)

    def rotateLeft(step: Int): B[A] =
      rotateRight(-step)

    def startAt(i: IndexO): B[A] =
      rotateLeft(i)

    def reflectAt(i: IndexO = 0): B[A] =
      startAt(i + 1).reverse

    def segmentLengthO(p: A => Boolean, from: IndexO = 0): Int =
      startAt(from).segmentLength(p)

    private def multiply(times: Int): B[A] =
      Seq.fill(times)(ring).flatten.asInstanceOf[B[A]]

    def sliceO(from: IndexO, to: IndexO): B[A] =
      if from >= to || ring.isEmpty then Seq.empty.asInstanceOf[B[A]]
      else
        val length = to - from
        val times = Math.ceil(length / ring.size).toInt + 1
        startAt(from).multiply(times).take(length)
 
    private def growBy(growth: Int): B[A] =
      sliceO(0, ring.size + growth)

    def containsSliceO(slice: Seq[A]): Boolean =
      growBy(slice.size - 1).containsSlice(slice)

    def indexOfSliceO(slice: Seq[A]): Index =
      growBy(slice.size - 1).indexOfSlice(slice)

    def lastIndexOfSliceO(slice: Seq[A]): Index =
      growBy(slice.size - 1).lastIndexOfSlice(slice)

    def lastIndexOfSliceO(slice: Seq[A], end: Index): Index =
      growBy(slice.size - 1).lastIndexOfSlice(slice, end)

    def slidingO(size: Int, step: Int = 1): Iterator[B[A]] =
      sliceO(0, step * (ring.size - 1) + size).sliding(size, step)

    def allRotations: Iterator[B[A]] =
      slidingO(ring.size)

    def allRotationsAndReflections: Iterator[B[A]] =
      (allRotations ++ ring.reverse.allRotations)

    def minRotation(implicit ordering: Ordering[B[A]]): B[A] =
      allRotations.min(ordering)

    def isRotationOf(other: Seq[A]): Boolean =
      allRotations.contains(other)

    def isReflectionOf(other: Seq[A]): Boolean =
      ring == other || ring.reflectAt() == other

    def isRotationOrReflectionOf(other: Seq[A]): Boolean =
      val reflected = other.reverse
      allRotations.exists(r => r == other || r == reflected)

    private def areFoldsSymmetrical: Int => Boolean =
      n => rotateRight(ring.size / n) == ring

    def rotationalSymmetry: Int =
      val size = ring.size
      if size < 2 then 1
      else
        val exactFoldsDesc = size +: (size / 2 to 2 by -1).filter(size % _ == 0)
        exactFoldsDesc.find(areFoldsSymmetrical).getOrElse(1)

    private def greaterHalfSize: Int =
      Math.ceil(ring.size / 2.0).toInt

    private def checkReflectionAxis(gap: Int): Boolean =
      (0 until greaterHalfSize).forall(j => applyO(j + 1) == applyO(-(j + gap)))

    private def hasHeadOnAxis: Boolean =
      checkReflectionAxis(1)

    private def hasAxisBetweenHeadAndNext: Boolean =
      checkReflectionAxis(0)

    private def findReflectionSymmetry: Option[Index] =
      (0 until greaterHalfSize).find(j =>
        val rotation = startAt(j)
        rotation.hasHeadOnAxis || rotation.hasAxisBetweenHeadAndNext
      )
   
    def symmetryIndices: List[Index] =
      if ring.isEmpty then Nil
      else
        val folds = rotationalSymmetry
        val foldSize = ring.size / folds
        ring.take(foldSize).findReflectionSymmetry match
          case None => Nil
          case Some(j) => (0 until folds).toList.map(_ * foldSize + j)

    def symmetry: Int =
      symmetryIndices.size
