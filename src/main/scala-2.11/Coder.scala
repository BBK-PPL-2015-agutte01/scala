package playfair

import scala.annotation.tailrec

class Coder(var keyword: String) {
  keyword = format(keyword)
  val ALPHABET = "ABCDEFGHIKLMNOPQRSTUVWXYZ" // no J

  def encode(plainText: String): String = {
    // val pairs = processText(plainText)
    // encipher(pairs)
    "Oh hey pretend this is the encoded result."
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
    val uc = uniqueChars(keyword + ALPHABET)

    val keyTable = Array(uc.slice(0, 5),
                         uc.slice(5, 10),
                         uc.slice(10, 15),
                         uc.slice(15, 20),
                         uc.slice(20, 25))
    keyTable
  }

  /** Formats a String ready to encode or use in the key table
   *
   * @param input text, String
   * @return a String with only upper case alphabet Chars, and all 'J's replaced with 'I's
   */
  def format(input: String): String = {
    input.toUpperCase.replaceAll("[^a-zA-Z]", "").replaceAll("J", "I")
  }

  /** Creates an array of unique characters from String
    *
    * @param word text, String
    * @return array of unique characters
    */
  def uniqueChars(word: String): Array[Char] =
    word.toArray.foldLeft(Array[Char]())((a, b) => if (a.contains(b)) a else a :+ b)

  /** Formats input text according to the Playfair rules.
    *
    * ~ All pairs of letters will be separated by spaces,
    * ~ If a double letter occurs in a pair, an 'X' is inserted between them,
    * ~ If a pair is "XX", a 'Q' is inserted between them.
    * ~ If there are an odd number of letters the last will be paired with 'Z',
    *
    * @param text String of only A-Z and upper case Chars
    * @return formatted String
    */
  def processText(text:String): String = {

    @tailrec
    def processHelper(acc: Array[Char], text: String): Array[Char] = {
      if (text.length == 0) acc
      else if (text.length == 1) 'Z' +: (text.head +: acc)
      else {
        if (text.head == 'X' && text.tail.head == 'X')
          processHelper(' ' +: ('Q' +: ('X' +: acc)), text.tail)
        else if (text.head == text.tail.head)
          processHelper(' ' +: ('X' +: (text.head +: acc)), text.tail)
        else processHelper(' ' +: (text.tail.head +: (text.head +: acc)), text.tail.tail)
      }
    }

    new String(processHelper(Array[Char](), text).reverse)
  }

}

