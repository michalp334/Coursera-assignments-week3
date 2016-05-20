package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s0 = singletonSet(0)
    val sN1 = singletonSet(-1)
    val sN2 = singletonSet(-2)
    val u1 = union(s1, s2) //(1,2)
    val u2 = union(s3, s4) //(3,4)
    val u3 = union(sN1, sN2) //(-1,-2)
    val u4 = union(u1, u3) //(-1,-2,1,2)
    val u5 = union(u4, u2) //(-1,-2,1,2,3,4)
    val u6 = union(u5, s0) //(-1,-2,0,1,2,3,4)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton1")
      assert(!contains(s2, -2), "Singleton2")
      assert(!contains(s3, 1), "Singleton3")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains common elements of both sets") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")

      val t = intersect(s1, u1)
      assert(contains(t, 1), "Intersect 3")
      assert(!contains(t, 2), "Intersect 4")
    }
  }

  test("diffrence contains elements of first set that are not matched in the second one") {
    new TestSets {
      val s = diff(u1, s1)
      assert(contains(s, 2), "Diff 1")
      assert(!contains(s, 1), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter from set(-2,-1,1,2,3,4) for even numbers should return set(-2,2,4)") {
    new TestSets {
      val s = filter(u5, x => x % 2 == 0)
      assert(contains(s, 2), "Filter 1")
      assert(contains(s, 4), "Filter 2")
      assert(contains(s, -2), "Filter 3")
      assert(!contains(s, 1), "Filter 4")
      assert(!contains(s, -1), "Filter 5")
    }
  }

  test("Forall should return true for predicates that are true for every element of the set and false otherwise") {
    new TestSets {
      assert(forall(u5, x => x < 10), "Forall 1")
      assert(forall(u5, x => x > -3), "Forall 2")
      assert(!forall(u5, x => x > 0), "Forall 3")

    }
  }

  test("Exists should return true if at least one element fulfill predicate and false otherwise") {
    new TestSets {
      assert(exists(u5, x => x == -1), "Exists 1")
      assert(exists(u5, x => x > 0), "Exists 2")
      assert(!exists(u5, x => x > 10), "Exists 3")

    }
  }

  test("Map of (-2,-1,0,1,2,3,4) and multiplying by 2 should (-4,-2,0,2,4,6,8)") {
    new TestSets {
      val s = map(u6, x => x * 2)
      assert(contains(s, 0), "Map 1")
      assert(contains(s, -4), "Map 2")
      assert(contains(s, 8), "Map 3")
      assert(!contains(s, -1), "Map 4")
      assert(!contains(s, 3), "Map 5")

    }
  }
}
