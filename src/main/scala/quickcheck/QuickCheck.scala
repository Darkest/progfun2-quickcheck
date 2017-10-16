package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = oneOf(
    const(this.empty),
    for {
      k <- arbitrary[this.A]
      m <- oneOf(const(this.empty), genHeap)
    } yield insert(k, m)
  )
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: this.A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a1: this.A, a2: this.A) =>
    val h = insert(a2, insert(a1, empty))
    findMin(h) == (if (a1 < a2) a1 else a2)
  }

  property("min3") = forAll { (h1: this.H, h2: this.H) =>
    val h = meld(h1, h2)
    val h1min: Option[A] = if (isEmpty(h1)) None else Some(findMin(h1))
    val h2min: Option[A] = if (isEmpty(h2)) None else Some(findMin(h2))
    if (!isEmpty(h)) {
      val min = (h1min, h2min) match {
        case (None, Some(x)) => x
        case (Some(x), None) => x
        case (Some(x), Some(y)) =>
          if (x < y) x else y
      }
      (findMin(h) == min) || (findMin(h) == min)
    }
    else true
  }

  property("emp1") = forAll { (a1: this.A) =>
    val h = deleteMin(insert(a1, empty))
    isEmpty(h)
  }

  property("order") = forAll { h: this.H =>
    def list(h: H): List[A] = {
      @tailrec
      def listHelper(h: H, res: List[A]): List[A] = {
        if (isEmpty(h)) Nil
        else {
          val h1 = deleteMin(h)
          val t = findMin(h)
          listHelper(h1, t :: res)
        }
      }

      listHelper(h, List())
    }

    list(h).sorted == list(h)
  }

}
