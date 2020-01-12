package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      m <- genHeap
    } yield insert(v, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of 2") = forAll { (a: Int, b: Int) =>
    val actual = insert(a, insert(b, empty))

    findMin(actual) == Math.min(a, b)
  }

  property("insert into empty heap") = forAll { a: Int =>
    val h = insert(a, empty)
    val actual = deleteMin(h)
    isEmpty(actual)
  }

  property("sorted heap") = forAll { h: H =>
    isSorted(h)
  }

  property("minimum of melding") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin(meld(h1, h2)) == findMin(h2)
    else if (isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
    else findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("sorted melded heap") = forAll { (h1: H, h2: H) =>
    isSorted(meld(h1, h2))
  }

  property("sorted deleted heap") = forAll { h1: H =>
    if (isEmpty(h1)) true
    else isSorted(deleteMin(h1))
  }

  property("sorted inserted heap") = forAll { (h1: H, e: Int) =>
    isSorted(insert(e, h1))
  }

  property("insert minimum") = forAll { h1: H =>
    findMin(insert(Int.MinValue, h1)) == Int.MinValue
  }

  property("insert delete") = forAll { h1: H =>
    if (isEmpty(h1)) {
      val actual = insert(Int.MinValue, h1)
      isEmpty(deleteMin(actual))
    } else {
      val prevMin = findMin(h1)

      if (prevMin == Int.MinValue) true
      else {
        val actual = insert(Int.MinValue, h1)
        equals(deleteMin(actual), h1)
      }
    }
  }

  property("insert associativity") = forAll { (a: Int, b: Int) =>
    findMin(insert(a, insert(b, empty))) == findMin(insert(b, insert(a, empty)))
  }

  property("meld commutativity") = forAll { (h1: H, h2: H) =>
    equals(meld(h1, h2), meld(h2, h1))
  }

  @tailrec
  private def isSorted(heap: H, prev: Int = Int.MinValue): Boolean =
    if (isEmpty(heap)) true
    else {
      val curr = findMin(heap)
      val subHeap = deleteMin(heap)
      curr >= prev && isSorted(subHeap, curr)
    }

  @tailrec
  private def equals(f: H, t: H): Boolean =
    if (isEmpty(f) || isEmpty(t)) isEmpty(f) && isEmpty(t)
    else {
      val curr1 = findMin(f)
      val subHeap1 = deleteMin(f)
      val curr2 = findMin(t)
      val subHeap2 = deleteMin(t)

      curr1 == curr2 && equals(subHeap1, subHeap2)
    }
}
