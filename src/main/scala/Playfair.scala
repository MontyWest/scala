package playfair

import scala.io.StdIn

/**
  *@author Monty West - mwest06
  */
object Playfair {
  val ENCODE_NUM = 1
  val DECODE_NUM = 2
  val QUIT_NUM = 3
  
  val welcome = "Welcome to the PlayFair cipher machine!"
  val goodbye = "Goodbye!\n"
  val message = "\n-----------------------------\n" +
                "\t" + ENCODE_NUM + ". Encode...\n" +
                "\t" + DECODE_NUM + ". Decode...\n" +
                "\t" + QUIT_NUM + ". Quit.\n\n" +
                "Please choose an option (as a number) from above:"
    
  def main(args: Array[String]) = {
    var run = true
    
    println(welcome)              
    do {
      println(message)
      try {
        val choice = StdIn.readInt()
        println("")
        choice match {
          case ENCODE_NUM => runEncode
          case DECODE_NUM => runDecode
          case QUIT_NUM => run = false
          case _ => "That wasn't an option!"
        }
      } catch {
        case e: NumberFormatException => println("That's not a number!")
      }
    } while(run && {
        println("\n(Press enter to continue)")
        StdIn.readLine()
        true
      })
      
    println(goodbye)
  }
  
  def runEncode: Unit = runAction(Coder(getKeywordFromUser).encode, "Encoding...")
  def runDecode: Unit = runAction(Coder(getKeywordFromUser).decode, "Decoding...")
  
  def runAction(action: String => String, actionName: String): Unit = {
    try {
      val text: String = getTextFromFile
      println("File read successful.")
      println(actionName + "\n")
      val returnedText: String = action(text)
      println("Result:")
      print(returnedText)
    } catch {
      case e: Exception => println("Error reading from file: " + e.getMessage)
    }
  }
  
  def getKeywordFromUser: String = {
    print("\nEnter keyword: ");
    val keyword: String = StdIn.readLine()
    if (keyword == null) "" else keyword
  }
  
  def getTextFromFile: String = {
    print("\nEnter filename (including .txt extension): ");
    val filename: String = StdIn.readLine()
    val filenameSafe = if (filename == null) "file.txt" else filename
    
    import scala.io.Source
    val file = Source.fromFile(filenameSafe)
    val text = try {
      file.mkString
    } catch {
      case e: Exception => throw e
    } finally {
      file.close()
    }
    text
  }
}