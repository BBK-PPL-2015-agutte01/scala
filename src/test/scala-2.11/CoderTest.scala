import org.scalatest.{Matchers, FlatSpec}

class CoderTest extends FlatSpec with Matchers {

  val coder = new Coder("")

  "A Coder object " should "call the prepare method upon instantiation, so that the keyword has no " +
    "non-alphabet characters, is upper case, and any J's are replaced by I's" in {
    val c = new Coder("adsjg234j34897/£/@/£/$J/:DFLGasdhgd")
    c.keyword should be("adsigiidflgasdhgd")
  }

  "An generateKeyTable method " should "return a Map with Chars as keys as (column, row) tuples as values," +
    " returning (0, 1) for 'a'." in {
    new Coder("").generateKeyTable('a') should be (0, 0)
  }

  "A processText method " should "insert a q in the tuple if there are two x's in a pair." in {
    coder.processInput("xx") should be (Array(('x', 'q'),('x', 'z')))
  }

  it should "insert an x if there are two identical letters in a row." in {
    coder.processInput("aaa") should be (Array(('a', 'x'), ('a', 'x'), ('a', 'z')))
  }

  it should "allow for two letters in a row, in separate tuples." in {
    coder.processInput("laab") should be (Array(('l', 'a'),('a', 'b')))
  }

  "An swapLetters method " should "return encoded letters as a String when the second arg is +1." in {
    coder.swapLetters(Array(('a', 'e'), ('a', 'k'), ('a', 'v')), 1) should be ("baeffa")
  }

  it should "return decoded letters as a String when the second arg is -1." in {
    coder.swapLetters(Array(('b', 'a'), ('e', 'f'), ('f', 'a')), -1) should be ("aeakav")
  }


}
