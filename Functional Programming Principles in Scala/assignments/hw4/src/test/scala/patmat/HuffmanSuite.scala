package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Leaf('a', 1)
	}

  trait TestCodeTables {
    val c1 = List(('a', List(0)), ('b', List(1)))
    val c2 = List(('a', List(0, 0)), ('b', List(0, 1)), ('c', List(1)))
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times: This function computes for each unique character in the list `chars` the number of times it occurs") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)) || times(List('a', 'b', 'a')) === List(('b', 1), ('a', 2)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton: whether the list `trees` contains only one single code tree") {
    new TestTrees {
      assert(singleton(List(t1)) === true)
      assert(singleton(List(t1, t2)) === false)
    }
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree") {
    val chars_1 = List('b', 'b', 'a', 'a', 'b')
    val chars_2 = List('a', 'b', 'd', 'b', 'a', 'd', 'd', 'b', 'd')
    new TestTrees {
      assert(createCodeTree(chars_1) === t1)
      assert(createCodeTree(chars_2) === t2)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(encode(t3)(List('a')) === List(0))
      assert(encode(t1)("ab".toList) === List(0, 1))
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(encode(t2)("dba".toList) === List(1, 0, 1, 0, 0))
      assert(decode(t2, encode(t2)("dba".toList)) === "dba".toList)
    }
  }

  test("code bits") {
    new TestCodeTables {
      assert(codeBits(c1)('a') === List(0))
      assert(codeBits(c1)('b') === List(1))
      assert(codeBits(c2)('a') === List(0, 0))
      assert(codeBits(c2)('b') === List(0, 1))
      assert(codeBits(c2)('c') === List(1))
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('c', List(1))))
      assert(List(('a', encode(t3)(List('a')))) === List(('a', List(0))))
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(quickEncode(t2)("dba".toList) === List(1, 0, 1, 0, 0))
      assert(decode(t2, quickEncode(t2)("dba".toList)) === "dba".toList)
    }
  }
}
