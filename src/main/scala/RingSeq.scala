object RingSeq {

  /* for improved readability, a Vector index */
  type Index = Int

  /* and a RingVector index, any value is valid */
  type IndexO = Int

  private def emptied[A](seq: Seq[A]): Seq[A] =
    seq.take(0)

  private def multiply[A](seq: Seq[A], times: Int): Seq[A] =
    (0 until times).foldLeft(emptied(seq))((acc, _) => acc ++ seq)

  private def greaterHalfSize(size: Int): Int =
    Math.ceil(size / 2.0).toInt

  private def checkReflectionAxis[A](seq: Seq[A], gap: Int): Boolean =
    (0 until greaterHalfSize(seq.size)).forall(j => seq.applyO(j + 1) == seq.applyO(-(j + gap)))

  private def hasHeadOnAxis[A](seq: Seq[A]): Boolean =
    checkReflectionAxis(seq, 1)

  private def hasAxisBetweenHeadAndNext[A](seq: Seq[A]): Boolean =
    checkReflectionAxis(seq, 0)

  private def findReflectionSymmetry[A](seq: Seq[A]): Option[Index] =
    (0 until greaterHalfSize(seq.size)).find(j => {
      val rotation = seq.startAt(j)
      hasHeadOnAxis(rotation) || hasAxisBetweenHeadAndNext(rotation)
    })

  implicit class RingSeqEnrichment[A](ring: Seq[A]) {

    private def index(i: IndexO): Index =
      java.lang.Math.floorMod(i, ring.size)

    def applyO(i: IndexO): A =
      ring(index(i))

    def rotateRight(step: Int): Seq[A] =
      if (ring.isEmpty) ring
      else {
        val j: Index = ring.size - index(step)
        ring.drop(j) ++ ring.take(j)
      }

    def rotateLeft(step: Int): Seq[A] =
      rotateRight(-step)

    def startAt(i: IndexO): Seq[A] =
      rotateLeft(i)

    def reflectAt(i: IndexO = 0): Seq[A] =
      startAt(i + 1).reverse

    def segmentLengthO(p: A => Boolean, from: IndexO = 0): Int =
      startAt(from).segmentLength(p)

    def sliceO(from: IndexO, to: IndexO): Seq[A] = {
      if (ring.isEmpty) ring
      else if (from >= to) emptied(ring)
      else {
        val length = to - from
        val times = Math.ceil(length / ring.size).toInt + 1
        multiply(startAt(from), times).take(length)
      }
    }

    private def growBy(growth: Int): Seq[A] =
      sliceO(0, ring.size + growth)

    def containsSliceO(slice: Seq[A]): Boolean =
      growBy(slice.size - 1).containsSlice(slice)

    def indexOfSliceO(slice: Seq[A]): Index =
      growBy(slice.size - 1).indexOfSlice(slice)

    def lastIndexOfSliceO(slice: Seq[A]): Index =
      growBy(slice.size - 1).lastIndexOfSlice(slice)

    def lastIndexOfSliceO(slice: Seq[A], end: Index): Index =
      growBy(slice.size - 1).lastIndexOfSlice(slice, end)

    def slidingO(size: Int, step: Int = 1): Iterator[Seq[A]] =
      sliceO(0, step * (ring.size - 1) + size).sliding(size, step)

    private def transformations(f: Seq[A] => Iterator[Seq[A]]): Iterator[Seq[A]] =
      if (ring.isEmpty) Iterator(ring) else f(ring)

    def rotations: Iterator[Seq[A]] =
      transformations(r => slidingO(r.size))

    def reflections: Iterator[Seq[A]] =
      transformations(r => List(r, r.reflectAt()).iterator)

    def reversions: Iterator[Seq[A]] =
      transformations(r => List(r, r.reverse).iterator)

    def rotationsAndReflections: Iterator[Seq[A]] =
      transformations(_.reflections.flatMap(_.rotations))

    def minRotation(implicit ordering: Ordering[Seq[A]]): Seq[A] =
      rotations.min(ordering)

    private def isTransformationOf(other: Seq[A], f: Seq[A] => Iterator[Seq[A]]): Boolean =
      ring.sizeCompare(other) == 0 && f(ring).contains(other)

    def isRotationOf(other: Seq[A]): Boolean =
      isTransformationOf(other, _.rotations)

    def isReflectionOf(other: Seq[A]): Boolean =
      isTransformationOf(other, _.reflections)

    def isRotationOrReflectionOf(other: Seq[A]): Boolean =
      isTransformationOf(other, _.rotationsAndReflections)

    private def areFoldsSymmetrical: Int => Boolean =
      n => rotateRight(ring.size / n) == ring

    def rotationalSymmetry: Int = {
      val size = ring.size
      if (size < 2) 1
      else {
        val exactFoldsDesc = size +: (size / 2 to 2 by -1).filter(size % _ == 0)
        exactFoldsDesc.find(areFoldsSymmetrical).getOrElse(1)
      }
    }

    def symmetryIndices: List[Index] =
      if (ring.isEmpty) Nil
      else {
        val folds = rotationalSymmetry
        val foldSize = ring.size / folds
        findReflectionSymmetry(ring.take(foldSize)) match {
          case None => Nil
          case Some(j) => (0 until folds).toList.map(_ * foldSize + j)
        }
      }

    def symmetry: Int =
      symmetryIndices.size

  }

  implicit class RingStringEnrichment(s: String) {

    private def ring: IndexedSeq[Char] =
      s.toIndexedSeq

    def applyO(i: IndexO): Char =
      ring.applyO(i)

    def rotateRight(step: Int): String =
      ring.rotateRight(step).mkString

    def rotateLeft(step: Int): String =
      ring.rotateLeft(step).mkString

    def startAt(i: IndexO): String =
      ring.startAt(i).mkString

    def reflectAt(i: IndexO = 0): String =
      ring.reflectAt(i).mkString

    def segmentLengthO(p: Char => Boolean, from: IndexO = 0): Int =
      ring.segmentLengthO(p, from)

    def sliceO(from: IndexO, to: IndexO): String =
      ring.sliceO(from, to).mkString

    def containsSliceO(slice: String): Boolean =
      ring.containsSliceO(slice.toIndexedSeq)

    def indexOfSliceO(slice: String): Index =
      ring.indexOfSliceO(slice.toIndexedSeq)

    def lastIndexOfSliceO(slice: String): Index =
      ring.lastIndexOfSliceO(slice.toIndexedSeq)

    def lastIndexOfSliceO(slice: String, end: Index): Index =
      ring.lastIndexOfSliceO(slice.toIndexedSeq, end)

    def slidingO(size: Int, step: Int = 1): Iterator[String] =
      ring.slidingO(size, step).map(_.mkString)

    def rotations: Iterator[String] =
      ring.rotations.map(_.mkString)

    def reflections: Iterator[String] =
      ring.reflections.map(_.mkString)

    def reversions: Iterator[String] =
      ring.reversions.map(_.mkString)

    def rotationsAndReflections: Iterator[String] =
      ring.rotationsAndReflections.map(_.mkString)

    def minRotation(implicit ordering: Ordering[Seq[Char]]): String =
      ring.minRotation(ordering).mkString

    def isRotationOf(other: String): Boolean =
      ring.isRotationOf(other.toIndexedSeq)

    def isReflectionOf(other: String): Boolean =
      ring.isReflectionOf(other.toIndexedSeq)

    def isRotationOrReflectionOf(other: String): Boolean =
      ring.isRotationOrReflectionOf(other.toIndexedSeq)

    def rotationalSymmetry: Int =
      ring.rotationalSymmetry

    def symmetryIndices: List[Index] =
      ring.symmetryIndices

    def symmetry: Int =
      ring.symmetry

  }

}
