import org.scalatest.{Matchers, FlatSpec}

import playfair.{Coder, Playfair}

class CoderTest extends FlatSpec with Matchers {

  "A uniqueletters method " should "return ABCDE when given ABCDEEDCBA" in {
    val c = new Coder("ABCDE")
    c.uniqueChars("AABBCCDEEEEAAAAA") should be(Array[Char]('A','B','C','D','E'))
  }

  "A Coder object " should "call the alphabetAndUpperCase method upon instantiation, " +
    "so that the keyword has no non-alphabet characters and is upper case" in {
    val c = new Coder("adsjg234j34897/£/@/£/$J/:DFLGasdhgd")
    c.keyword should be("ADSJGJJDFLGASDHGD")
  }

}
