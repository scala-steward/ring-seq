# **RingVector**
Extends Scala3 [`immutable.Vector`](https://dotty.epfl.ch/api/scala/collection/immutable/Vector.html) with ring (circular) methods.

```scala
import RingVector.*

"RING".toVector.rotateRight(1) // Vector(G,R,I,N)
```

## How to use
Import the [`RingVector`](/src/main/scala/RingVector.scala) object
where a `Vector` has to be considered circular.

## Need
Whenever data are structured in a circular sequence,
chances are you don't want to locally reinvent the wheel (pun intended).

## Solution
**RingVector** is a small, purely functional, self-contained library,
where most of the circular use cases are already solved
and building blocks provided for the others.

Leveraging Scala3 [`extension`](https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html),
it acts like a _decorator_,
providing new circular methods to `Vector`.

## Methods

### Circular version of existing ones
Named as their standard non-circular `Vector` alternatives,
but with an `O` suffix (meaning _ring_).

They are (see [test cases](/src/test/scala/OMethodsSpec.scala)):
* `applyO`
* `segmentLengthO`
* `sliceO`
* `containsSliceO`
* `indexOfSliceO`
* `lastIndexOfSliceO`
* `slidingO`

### Rotation and reflection
Rotate and reflect a circular `Vector`
(see [test cases](/src/test/scala/RotationsReflectionsSpec.scala))

### Symmetry
Calculate rotational and reflectional symmetries of a circular `Vector`
(see [test cases](/src/test/scala/SymmetriesSpec.scala))
