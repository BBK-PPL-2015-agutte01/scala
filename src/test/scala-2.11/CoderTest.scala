import org.scalatest.{Matchers, FlatSpec}

import playfair.{Coder, Playfair}

class CoderTest extends FlatSpec with Matchers {

  val coder = new Coder("")

  "A uniqueChars method " should "return ABCDE when given AABBCCDEEEEAAAAA" in {
    coder.uniqueChars("AABBCCDEEEEAAAAA") should be(List[Char]('A','B','C','D','E'))
  }

  "A Coder object " should "call the alphabetAndUpperCase method upon instantiation, " +
    "so that the keyword has no non-alphabet characters, is upper case, and any J's are replaced by I's" in {
    val c = new Coder("adsjg234j34897/£/@/£/$J/:DFLGasdhgd")
    c.keyword should be("ADSIGIIDFLGASDHGD")
  }

  "An unflatten method " should "return a 5x5 2D array when given an array of 25 elements." in {
    coder.unflatten(Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
      'r', 's', 't', 'u', 'v', 'w', 'x', 'y')) should be (Array(Array('a', 'b', 'c', 'd', 'e'),
                                                                Array('f', 'g', 'h', 'i', 'j'),
                                                                Array('k', 'l', 'm', 'n', 'o'),
                                                                Array('p', 'q', 'r', 's', 't'),
                                                                Array('u', 'v', 'w', 'x', 'y')))
  }

}
