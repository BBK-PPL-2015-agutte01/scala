package playfair

import scala.annotation.tailrec

class Coder(var keyword: String) {

  /** Upon instantiation of the class */
  keyword = format(keyword)
  val keytable = generateKeyTable

  def encode(plainText: String): String = {
    val pairs = processText(plainText)
    encipher(pairs)
  }

  def decode(secretText: String): String = ???


  /** Creates a 5x5 key table from the keyword.
    *
    * Throws an IllegalArgumentException if the array does not have 25 elements.
    *
    * @return a 5x5 key table with the keyword and the rest of the alphabet
    *         (excluding 'J') as unique Chars
    */
  def generateKeyTable = {
    val uc = uniqueChars(keyword + Coder.ALPHABET)

    Array(uc.slice(0, 5),
          uc.slice(5, 10),
          uc.slice(10, 15),
          uc.slice(15, 20),
          uc.slice(20, 25))
  }

  /** Formats a String ready to encode or use in the key table
   *
   * @param input text, String
   * @return a String with only lower case alphabet Chars, and all 'J's replaced with 'I's
   */
  def format(input: String): String = {
    input.toLowerCase.replaceAll("[^a-zA-Z]", "").replaceAll("j", "i")
  }

  /** Creates an array of unique characters from String
    *
    * @param word text, String
    * @return array of unique characters
    */
  def uniqueChars(word: String): Array[Char] =
    word.toArray.foldLeft(Array[Char]())((a, b) => if (a.contains(b)) a else a :+ b)

  /** Creates tuples according to the Playfair rules.
    *
    * ~ All pairs of letters will be separated by spaces,
    * ~ If a double letter occurs in a pair, an 'X' is inserted between them,
    * ~ If a pair is "XX", a 'Q' is inserted between them.
    * ~ If there are an odd number of letters the last will be paired with 'Z',
    *
    * @param text String of only A-Z and lower case Chars
    * @return Array of Char tuples
    */
  def processText(text:String): Array[(Char, Char)] = {

    @tailrec
    def processHelper(acc: Array[(Char, Char)], text: String): Array[(Char, Char)] = {
      if (text.length == 0) acc
      else if (text.length == 1) (text.head, 'z') +: acc
      else {
        if (text.head == 'x' && text.tail.head == 'x')
          processHelper(('x', 'q') +: acc, text.tail)
        else if (text.head == text.tail.head)
          processHelper((text.head, 'x') +: acc, text.tail)
        else processHelper((text.head, text.tail.head) +: acc, text.tail.tail)
      }
    }

    processHelper(Array[(Char, Char)](), text.toLowerCase).reverse
  }

  def addSpaces(interval: Int, text: String): String = ???
//    def spacesHelper(acc: Array[Char], text: Array[Char]) = {
//      if (text.length < 2) text +: acc
//      else
//    }
//
//    var a = text.toCharArray
//    var result = Array[Char]()
//    for (i <- a.indices; e <- a) {
//      if ((i % interval) - interval + 1 == 0) result = result ++ a.slice(0, i) ++ (' ' +: a.slice(i, a.length))
//      println(new String(a))
//    }
//    new String(a)
//  }

  def encipher(pairs: Array[(Char, Char)]):String = {
    ???
  }

}

object Coder {
  val ALPHABET = "ABCDEFGHIKLMNOPQRSTUVWXYZ" // no J
  def apply(keyword: String) = new Coder(keyword)
}