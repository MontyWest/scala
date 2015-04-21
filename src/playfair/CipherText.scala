package playfair

class CipherText(val message: List[(Char, Char)]) {
  
  override def toString = {
    message.flatMap {x => Array(x._1, x._2)}
           .grouped(CipherText.WORD_LENGTH).grouped(CipherText.LINE_LENGTH)
           .map { x => x.map { y => y.mkString(CipherText.CHAR_SEP) }
                        .mkString(CipherText.WORD_SEP)}
           .mkString(CipherText.LINE_SEP)
  }  
}

object CipherText {
  val WORD_LENGTH = 5;
  val LINE_LENGTH = 10;
  val LINE_SEP = "\n";
  val WORD_SEP = " ";
  val CHAR_SEP = "";
  
  /***
   * Expects a string containing only lowercase letters
   */
  def apply(str: String): CipherText = {
    //What if last group only has one element? (Shouldn't occur in coded text)
    new CipherText(str.toList.grouped(2).map { x => (x(0), x(1)) } toList)
  }
}