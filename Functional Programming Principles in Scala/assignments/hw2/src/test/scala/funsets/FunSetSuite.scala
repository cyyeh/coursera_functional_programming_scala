package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
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

  test("intersect contains the set of all elements that are both in `s` and `t`") {
    new TestSets {
      val s = union(s1, s2)
      val sIntersect1 = intersect(s1, s)
      val sIntersect2 = intersect(s2, s)
      val sIntersect3 = intersect(s1, s2)
      assert(contains(sIntersect1, 1), "Intersect 1")
      assert(contains(sIntersect2, 2), "Intersect 2")
      assert(!contains(sIntersect3, 1), "Intersect null")
      assert(!contains(sIntersect3, 2), "Intersect null")
    }
  }

  test("diff contains the set of all elements of `s` that are not in `t`") {
    new TestSets {
      var s = union(s1, s2)
      var sDiff = diff(s, s1)
      assert(contains(sDiff, 2), "Diff 2")
      assert(!contains(sDiff, 1), "Diff 2")
    }
  }

  test("filter contains the subset of `s` for which `p` holds") {
    new TestSets {
      var sUnion0 = union(s1, s2);
      var sUnion1 = union(sUnion0, s3);
      var sPositive = filter(sUnion1, x => x > 0)
      var sEven = filter(sUnion1, x => x % 2 == 0)
      var sOdd = filter(sUnion1, x => x % 2 == 1)
      var sBiggerThan1 = filter(sUnion1, x => x > 1)
      assert(contains(sPositive, 1), "sPositive contains 1")
      assert(contains(sPositive, 2), "sPositive contains 2")
      assert(contains(sPositive, 3), "sPositive contains 3")
      assert(contains(sEven, 2), "sEven contains 2")
      assert(!contains(sEven, 1), "sEven contains 2")
      assert(!contains(sEven, 3), "sEven contains 2")
      assert(!contains(sOdd, 2), "sOdd contains 1, 3")
      assert(!contains(sBiggerThan1, 1), "sBiggerThan1 contains 2, 3")
    }
  }

  test("forall that whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val bound = 1000
      var sUnion0 = union(s1, s2);
      var sUnion1 = union(sUnion0, s3);
      var forAllResult = forall(sUnion1, x => x > 0)
      assert(forAllResult, "should be true")
      var forAllResult2 = forall(sUnion1, x => x > 1)
      assert(!forAllResult2, "should be false")
    }
  }

  test("exists that whether there exists a bounded integer within `s` that satisfies `p`") {
    new TestSets {
      val bound = 1000
      var sUnion0 = union(s1, s2);
      var sUnion1 = union(sUnion0, s3);
      var existsResult = exists(sUnion1, x => x == 1)
      assert(existsResult, "should be true")
      var existsResult1 = exists(sUnion1, x => x > 0)
      assert(existsResult1, "should be true")
      var existsResult2 = exists(sUnion1, x => x < 0)
      assert(!existsResult2, "should be false")
    }
  }

  test("map that a set transformed by applying `f` to each element of `s`") {
    new TestSets {
      var sUnion0 = union(s1, s2);
      var sUnion1 = union(sUnion0, s3);
      var sMapSet = map(sUnion1, x => x + 1)
      assert(!contains(sMapSet, 1), "contains 2, 3, 4")
      assert(contains(sMapSet, 4), "contains 2, 3, 4")
    }
  }
}
