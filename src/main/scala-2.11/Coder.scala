package playfair

class Coder(var keyword: String) {
  keyword = format(keyword)
  val ALPHABET = "ABCDEFGHIKLMNOPQRSTUVWXYZ" // no J

  def encode(plainText: String): String = {
    // val pairs = processText(plainText)
    // encipher(pairs)
    "Oh hey pretend this is the encoded result."
  }

  def decode(secretText: String): String = ???

  def generateKeyTable: Unit = {
    val uc = uniqueChars(keyword + ALPHABET)
    val keyTable = unflatten(uc)
  }

  def format(input: String): String = {
    input.toUpperCase.replaceAll("[^a-zA-Z]", "").replaceAll("J", "I")
  }

  def uniqueChars(word: String): Array[Char] =
    word.toArray.foldLeft(Array[Char]())((a, b) => if (a.contains(b)) a else a :+ b)

  def unflatten(arr: Array[Char]): Array[Array[Char]] = {
    Array(arr.slice(0, 5),
          arr.slice(5, 10),
          arr.slice(10, 15),
          arr.slice(15, 20),
          arr.slice(20, 25))
  }

//  def processText(text: String): String = {
//    def processHelper(acc: Array[Char], text: Array[Char]) = {
//
//      if (text.length == 0) acc
//      else if (text.length == 1) text.head +: ('z' +: acc)
//      else {
//        if (text.head == 'x' && text.tail.head == 'x') processHelper()
//      }
//      (first == 'x' && second == 'x')
//    }
//
//    processHelper(, text.toArray).toString
//  }

}

