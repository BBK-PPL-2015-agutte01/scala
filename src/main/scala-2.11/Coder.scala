package playfair

import scala.annotation.tailrec

class Coder(var keyword: String) {

  /** Upon instantiation of the class */
  keyword = prepare(keyword)
  val charKeysTable = generateKeyTable
  val coordinateKeysTable = charKeysTable.map(_.swap)

  /** Calls prepare, processInput and swapLetters methods to encode input text
   *
   * @param plainText input text
   * @return multi-line String that has been encoded and formatted
   */
  def encode(plainText: String): String = {
    format(
      swapLetters(
        processInput(
          prepare(plainText)), 1))
  }

  /** Calls prepare on an Array of tuples, then swapLetters and format methods
    * to decode input text
    *
    * @param secretText input text
    * @return multi-line String that has been decoded and formatted
    */
  def decode(secretText: String): String = {
    format(
      swapLetters(
        prepare(secretText).toCharArray.grouped(2).map(a => (a(0), a(1))).toArray, -1))
  }

  def format(input: String): String = {
    input.zipWithIndex.foldLeft("")((acc, charAndIndex) =>
      if ((charAndIndex._2 + 1) % Coder.CHARS_PER_LINE == 0) {
        acc + charAndIndex._1.toString + "\n"
      }
      else if (charAndIndex._2 % Coder.CHARS_PER_BLOCK == 4) {
        acc + charAndIndex._1.toString + " "
      }
      else acc + charAndIndex._1.toString)
  }

  /** Creates a 5x5 key table from the keyword.
    *
    * Throws an IllegalArgumentException if the array does not have 25 elements.
    *
    * @return a 5x5 key table with the keyword and the rest of the alphabet
    *         (excluding 'J') as unique Chars
    */
  def generateKeyTable = {
    (keyword + Coder.ALPHABET)
      .foldLeft(Array[Char]())((acc, char) =>
        if (acc.contains(char)) acc else acc :+ char)
      .zip(Coder.numberArray).toMap
  }

  /** Formats a String ready to encode or use in the key table
   *
   * @param input text, String
   * @return a String with only lower case alphabet Chars, and all 'J's replaced with 'I's
   */
  def prepare(input: String): String = {
    input.toLowerCase.replaceAll("[^a-zA-Z]", "").replaceAll("j", "i")
  }

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
  def processInput(text:String): Array[(Char, Char)] = {

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

  /** Swaps letters according to the keytable.
   *
   * @param pairs Array of char tuples
   * @param shift either positive (1) or negative (-1) to cater for both
   *              encoding and decoding
   * @return String of swapped characters
   */
  def swapLetters(pairs: Array[(Char, Char)], shift: Int):String = {

    if (shift != 1 && shift != -1) throw new IllegalArgumentException("Illegal shift integer.")

    def getChar(c: Int, r: Int): Char = {
      if (c > Coder.KEYTABLE_SIZE) coordinateKeysTable(0, r)
      else if (r > Coder.KEYTABLE_SIZE) coordinateKeysTable(c, 0)
      else if (c < 0) coordinateKeysTable(Coder.KEYTABLE_SIZE, r)
      else if (r < 0) coordinateKeysTable(c, Coder.KEYTABLE_SIZE)
      else coordinateKeysTable(c, r)
    }

    val newPairs = pairs.map {
      // same column
      case (a, b) if charKeysTable(a)._1 == charKeysTable(b)._1 =>
        (getChar(charKeysTable(a)._1, charKeysTable(a)._2 + shift),
         getChar(charKeysTable(b)._1, charKeysTable(b)._2 + shift))
      // same row
      case (a, b) if charKeysTable(a)._2 == charKeysTable(b)._2 =>
        (getChar(charKeysTable(a)._1 + shift, charKeysTable(a)._2),
         getChar(charKeysTable(b)._1 + shift, charKeysTable(b)._2))
      case (a, b) =>
        (coordinateKeysTable(charKeysTable(b)._1, charKeysTable(a)._2),
         coordinateKeysTable(charKeysTable(a)._1, charKeysTable(b)._2))
    }

    newPairs.foldRight(""){ (pair, acc) => pair._1.toString + pair._2.toString + acc}
  }

}

object Coder {
  val ALPHABET = "abcdefghiklmnopqrstuvwxyz" // no J
  val numberArray = generateNumberArray()
  val KEYTABLE_SIZE = 4
  val CHARS_PER_BLOCK = 5
  val CHARS_PER_LINE = 50

  def apply(keyword: String) = new Coder(keyword)

  def generateNumberArray(): List[(Int, Int)] = {
    val listOf5 = List.range(0, 5) // creates List(0, 1, 2, 3, 4)
    val times5 = listOf5 ::: listOf5 ::: listOf5 ::: listOf5 ::: listOf5 // new List - concats list 5 times
    val many5s = listOf5.flatMap(n => List(n, n, n, n, n)) // new List - replicates each number 5 times
    times5.zip(many5s) // zips the previous 2 new Lists together
  }
}