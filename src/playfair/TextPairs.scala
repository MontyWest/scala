package playfair

class TextPairs(val message: List[(Char, Char)]) {
  

  
  override def toString = {
    message.flatMap {x => Array(x._1, x._2)}
           .grouped(TextPairs.WORD_LENGTH).grouped(TextPairs.LINE_LENGTH)
    .map { (x => x.map { (y => y.mkString(TextPairs.CHAR_SEP)) }
           .mkString(TextPairs.WORD_SEP))}
    .mkString(TextPairs.LINE_SEP)
  }  
}

object TextPairs {
  val WORD_LENGTH = 5;
  val LINE_LENGTH = 10;
  val LINE_SEP = "\n";
  val WORD_SEP = " ";
  val CHAR_SEP = "";
  
  /***
   * Expects a cipherText (encoded using Playfair) containing only lowercase letters
   */
  def fromCiphertext(ciphertext: String): TextPairs = {
    //What if last group only has one element? (Shouldn't occur in coded text)
    new TextPairs(ciphertext.toList.grouped(2).map { x => (x(0), x(1)) } toList)
  }

  def fromPlaintext(plaintext: String): TextPairs = ???
}