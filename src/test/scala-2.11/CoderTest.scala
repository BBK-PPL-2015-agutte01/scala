import org.scalatest.{Matchers, FlatSpec}

import playfair.{Coder, Playfair}

class CoderTest extends FlatSpec with Matchers {

  val coder = new Coder("")

  "A uniqueChars method " should "return abcde when given aabbccddeeeeaaaaa" in {
    coder.uniqueChars("aabbccddeeeeaaaaa") should be(List[Char]('a','b','c','d','e'))
  }

  "A Coder object " should "call the alphabetAndUpperCase method upon instantiation, " +
    "so that the keyword has no non-alphabet characters, is upper case, and any J's are replaced by I's" in {
    val c = new Coder("adsjg234j34897/£/@/£/$J/:DFLGasdhgd")
    c.keyword should be("adsigiidflgasdhgd")
  }

  "An generateKeyTable method " should "return a Map with Chars as keys as (column, row) tuples as values," +
    " returning (0, 1) for 'a'." in {
    new Coder("").generateKeyTable('a') should be (0, 0)
  }

  "A processText method " should "insert a q in the tuple if there are two x's in a pair." in {
    coder.processText("xx") should be (Array(('x', 'q'),('x', 'z')))
  }

  it should "insert an x if there are two identical letters in a row." in {
    coder.processText("aaa") should be (Array(('a', 'x'), ('a', 'x'), ('a', 'z')))
  }

  it should "allow for two letters in a row, in separate tuples." in {
    coder.processText("laab") should be (Array(('l', 'a'),('a', 'b')))
  }

}
