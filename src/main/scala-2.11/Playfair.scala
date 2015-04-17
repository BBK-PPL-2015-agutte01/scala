package playfair

import java.io.IOException
import scala.io.{Source, StdIn}

class Playfair {

  // def methodToTest: String = "hello"

  def startEncode = {
    val keyword = getKeyword

    val textToEncode = readFile
    println(s"The keyword is: $keyword, the text to encode is: $textToEncode")
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
        case e: IOException =>
          System.err.println("Error in reading file.")
      }
    } while (text == "")

    text
  }
}

object Playfair extends App {

  do {
    println("encode, decode, or quit?")
  } while(!readCommand(StdIn.readLine))

  def readCommand(command: String): Boolean = {
    command match {
      case "encode" =>
        val p = new Playfair
        p.startEncode
        true
      //case "decode" => startDecode; true
      //case "quit" => quit; true
      case _ => println("Sorry, your input was not understood."); false
    }
  }

}