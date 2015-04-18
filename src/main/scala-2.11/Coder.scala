package playfair

class Coder(var keyword: String) {
  keyword = alphabetAndUpperCase(keyword)

  def encode(plainText: String): String = {
    // generate keyword grid
    // val pairs = processText(plainText)
    // encipher(pairs)
    "Oh hey pretend this is the encoded result."
  }

  def decode(secretText: String): String = ???

  def generateKeyTable: Unit = {


//    var keyTable = Array() // 5x5 matrix
    uniqueChars(keyword)
  }

  def alphabetAndUpperCase(input: String): String = {
    input.replaceAll("[^a-zA-Z]", "").toUpperCase
  }

  def uniqueChars(word: String): List[Char] =
    word.toList.foldLeft(List[Char]())((a, b) => if (a.contains(b)) a else a :+ b)
}

