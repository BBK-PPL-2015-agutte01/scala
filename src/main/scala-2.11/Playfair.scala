package playfair

import java.io.IOException
import scala.io.{Source, StdIn}

class Playfair {

  // def methodToTest: String = "hello"

  def startEncode(keyword: String, textToEncode: String) = {
    println(s"The keyword is: $keyword, the text to encode is: $textToEncode")
    val c = new Coder(keyword)
    println(c.encode(textToEncode))
  }

  def getKeyword: String = {
    println("Please enter the keyword (only A-Z characters will be read)")
    StdIn.readLine
  }

  def readFile: String = {
    var text = ""

    do {
      println("please specify the filename (including extension):")
      val file = StdIn.readLine
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

  val p = new Playfair

  do {
    println("Please enter: (E)ncode, (D)ecode, or (Q)uit.")
  } while(!readCommand(StdIn.readLine))

  def readCommand(command: String): Boolean = {
    command.toUpperCase match {
      case "E" =>
        val keyword = p.getKeyword
        val textToEncode = p.readFile
        p.startEncode(keyword, textToEncode)
        false
      //case "decode" => startDecode; true
      case "Q" =>
        println("Thank you for using Playfair Cipher 2.0. Quitting now.")
        true
      case _ =>
        println("Sorry, your input was not understood.")
        false
    }
  }

}