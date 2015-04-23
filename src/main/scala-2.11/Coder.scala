package playfair

import scala.annotation.tailrec

class Coder(var keyword: String) {

  /** Upon instantiation of the class */
  keyword = format(keyword)
  val charKeysTable = generateKeyTable
  val coordinateKeysTable = charKeysTable.map(_.swap)

  def encode(plainText: String): String = {
    val unformatted = encipher(processText(format(plainText)))
    unformatted.toString
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

    uc.zip(Coder.numberArray).toMap
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
    word.foldLeft(Array[Char]())((a, b) => if (a.contains(b)) a else a :+ b)

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

  def encipher(pairs: Array[(Char, Char)]):String = {
    val newPairs = pairs.map {
      // same column
      case (a, b) if charKeysTable(a)._1 == charKeysTable(b)._1 =>
        (getChar(charKeysTable(a)._1, charKeysTable(a)._2 + 1),
         getChar(charKeysTable(b)._1, charKeysTable(b)._2 + 1))
      // same row
      case (a, b) if charKeysTable(a)._2 == charKeysTable(b)._2 =>
        (getChar(charKeysTable(a)._1 + 1, charKeysTable(a)._2),
         getChar(charKeysTable(b)._1 + 1, charKeysTable(b)._2))
      case (a, b) =>
        (coordinateKeysTable(charKeysTable(b)._1, charKeysTable(a)._2),
         coordinateKeysTable(charKeysTable(a)._1, charKeysTable(b)._2))
    }

    newPairs.foldRight(""){ (pair, acc) => pair._1.toString + pair._2.toString + acc}
  }

  def getChar(c: Int, r: Int): Char = {
    if (c > 4) coordinateKeysTable(0, r)
    else if (r > 4) coordinateKeysTable(c, 0)
    else coordinateKeysTable(c, r)
  }

}

object Coder {
  val ALPHABET = "abcdefghiklmnopqrstuvwxyz" // no J
  val numberArray = generateNumberArray

  def apply(keyword: String) = new Coder(keyword)

  def generateNumberArray(): List[(Int, Int)] = {
    val listOf5 = List.range(0, 5) // creates List(0, 1, 2, 3, 4)
    val times5 = listOf5 ::: listOf5 ::: listOf5 ::: listOf5 ::: listOf5 // new List - concats list 5 times
    val many5s = listOf5.flatMap(n => List(n, n, n, n, n)) // new List - replicates each number 5 times
    times5.zip(many5s) // zips the previous 2 new Lists together
  }
}