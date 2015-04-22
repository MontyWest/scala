package playfair

import scala.annotation.tailrec

class TextPairs(val underlying: List[(Char, Char)]) {
  
  def length = underlying.length
  def apply(i: Int) = underlying(i)
  
  def toList = underlying
  
  /**
   * Returns string representation of text pairs as prescribed by spec:
   *   Words of 5 letters each, separated with a space
   *   Lines contain 10 words
   */
  override def toString = {
    underlying.flatMap {x => Array(x._1, x._2)}
           .grouped(TextPairs.WORD_LENGTH).grouped(TextPairs.LINE_LENGTH)
           .map { (x => x.map { (y => y.mkString(TextPairs.CHAR_SEP)) }
                                       .mkString(TextPairs.WORD_SEP)) }
           .mkString(TextPairs.LINE_SEP)
  }  

}



object TextPairs {
	implicit def tpList(self: List[(Char, Char)]): TextPairs = new TextPairs(self)
  
  //Translation
  val END_BUFFER_CHAR = 'z'
  val SPLIT_DOUBLE_CHAR = 'x'
  val SPLIT_META_DOUBLE_CHAR = 'q'
  val REPLACE_CHAR = ('j','i')
  
  //Display
  val WORD_LENGTH = 5;
  val LINE_LENGTH = 10;
  val LINE_SEP = "\n";
  val WORD_SEP = " ";
  val CHAR_SEP = "";
  
  
  
  /***
   * Expects a cipherText (encoded using Playfair) containing only lowercase letters
   * Returns TextPairs of text as is (expects no double letters in pairs etc.)
   */
  def fromCiphertext(ciphertext: String): TextPairs = {
    //What if last group only has one element? (Shouldn't occur in coded text)
    ciphertext.toList.grouped(2).map { x => (x(0), x(1)) } toList
  }

  /***
   * Expects plaintext (unencoded) containing only lowercase letters (punctuation removed)
   * Returns TextPairs that obey the pair encoding rules of this Playfair variant
   */
  def fromPlaintext(plaintext: String): TextPairs = {
    
    def getDoubleBuffer(c: Char) = if (c == SPLIT_DOUBLE_CHAR) SPLIT_META_DOUBLE_CHAR else SPLIT_DOUBLE_CHAR
    
    /**
     * When carry is full then attach to acc
     * Otherwise add to carry from head of ptList
     * Exit when ptList and carry are empty
     */
    @tailrec
    def recuHelper(ptList: List[Char], acc: List[(Char, Char)] = List(), carry: (Option[Char], Option[Char]) = (None, None)): TextPairs = {
      
      (ptList, carry) match {
        // If carry contains two of the same letter, replace second with DOUBLE_BUFFER and add to acc. Carry letter to next.
        case ( _ , (Some(c), Some(d))) if c == d => recuHelper(ptList, (c, getDoubleBuffer(c)) :: acc, (Some(d), None))
        // If carry letters are different then add to acc, carry empty.
        case ( _ , (Some(c), Some(d))) if c != d => recuHelper(ptList, (c, d) :: acc, (None, None))
        
        // If ptList still has elements, and there's a partial carry. Complete carry with head and pass tail
        case ( ptH :: ptT, (Some(c), None)) => recuHelper(ptT, acc, (Some(c), Some(ptH)))
        // If ptList still has elements, and there's no carry. Carry head and pass tail
        case ( ptH :: ptT, (None, None)) => recuHelper(ptT, acc, (Some(ptH), None))
        
        // If ptList is empty and there's a partial carry, complete carry with END_BUFFER. (recursed in case c equals END_BUFFER)
        case (Nil, (Some(c), None)) => recuHelper(ptList, acc, (Some(c), Some(END_BUFFER_CHAR)))
        // If ptList is empty and there's no carry, return reversed acc
        case (Nil, (None, None)) => acc.reverse
      }
      
    }
    // As cipher runs on 25 letters, one needs to be replaced.
    recuHelper(plaintext.replace(REPLACE_CHAR._1, REPLACE_CHAR._2).toList)
  }
}