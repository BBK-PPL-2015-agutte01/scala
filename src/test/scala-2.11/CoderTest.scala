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

  "An generateKeyTable method " should "return a 5x5 2D array when given an array of 25 elements." in {
    new Coder("abcdefghijklmnopqrstuvwxyz").generateKeyTable should be (Array(Array('A', 'B', 'C', 'D', 'E'),
                                                                              Array('F', 'G', 'H', 'I', 'K'),
                                                                              Array('L', 'M', 'N', 'O', 'P'),
                                                                              Array('Q', 'R', 'S', 'T', 'U'),
                                                                              Array('V', 'W', 'X', 'Y', 'Z')))
  }

  "A processText method " should "return a String with space-separated pairs." in {
    coder.processText("ABCDE") should be ("AB CD EZ")
  }

  it should " insert a Q if there are two Xs in a pair." in {
    coder.processText("XX") should be ("XQ XZ")
  }

  it should "insert an X if there are two identical letters in a row." in {
    coder.processText("AAA") should be ("AX AX AZ")
  }

}
