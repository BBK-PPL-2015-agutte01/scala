import java.io.IOException
import scala.io.{Source, StdIn}

/** Entry point for the application. Reads input from the command line and
  * creates new instances of Coder to call encode or decode.
  *
  * @author Alice Gutteridge (agutte01)
  */
class Playfair() {

  def startEncode(keyword: String, textToEncode: String) = {
    println(s"\nThe keyword is: $keyword, the text to encode is:\n$textToEncode")
    val c = new Coder(keyword)
    println("ENCODED TEXT:")
    println(c.encode(textToEncode) + "\n")
  }

  def startDecode(keyword:String, textToDecode: String) = {
    println(s"\nThe keyword is: $keyword, the text to encode is:\n$textToDecode")
    val c = new Coder(keyword)
    println("DECODED TEXT:")
    println(c.decode(textToDecode) + "\n")
  }

  def readCommand(command: String): Boolean = {
    command.toUpperCase match {
      case "E" =>
        val keyword = getKeyword
        val textToEncode = readFile
        startEncode(keyword, textToEncode)
        false
      case "D" =>
        val keyword = getKeyword
        val textToDecode = readFile
        startDecode(keyword, textToDecode)
        false
      case "Q" =>
        println("Thank you for using Playfair Cipher 2.0. Quitting now.")
        true
      case _ =>
        println("Sorry, your input was not understood.")
        false
    }
  }

  def getKeyword: String = {
    println("Please enter the keyword (only A-Z characters will be read)")
    StdIn.readLine()
  }

  def readFile: String = {
    var text = ""

    do {
      println("please specify the filename (including extension):")
      val file = StdIn.readLine()
      try {
        text = Source.fromFile(file).mkString
      } catch {
        case e: IOException => System.err.println("Error in reading file.")
      }
    } while (text == "")

    text
  }

}

object Playfair extends App {

  /** Creates a new instance of Playfair.
   *
   * @return new Playfair instance
   */
  def apply() = new Playfair

  val p = Playfair()

  do {
    println("Please enter: (E)ncode, (D)ecode, or (Q)uit.")
  } while(!p.readCommand(StdIn.readLine()))

}