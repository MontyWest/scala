package playfair

class Coder(val keyword: String) {
  private var optCipher: Option[Cipher] = None;
  val cipher: Cipher = optCipher.getOrElse(createCipher)
  
  /**
   * Creates the cipher from the keyword
   */
  def createCipher(): Cipher = {
    val c = Cipher(Coder.removePunctuation(keyword))
    optCipher = Some(c)
    c
  }
  
  def decode(ciphertext: String): String = {
    val cipherPairs: TextPairs = TextPairs.fromCiphertext(Coder.removePunctuation(ciphertext));
    val plainishPairs: TextPairs = cipherPairs.toList.map(cipher.decodePair)
    plainishPairs.toString()
  }
  
  def encode(plaintext: String): String = {
    val plainPairs: TextPairs = TextPairs.fromPlaintext(Coder.removePunctuation(plaintext));
    val cipherPairs: TextPairs = plainPairs.toList.map(cipher.encodePair)
    cipherPairs.toString()
  }
}

object Coder {
  
  def apply(keyword: String): Coder = new Coder(keyword)
  
  /***
   * Removes all non-letters from string and makes it lowercase
   */
  def removePunctuation(str: String): String = {
    str.toLowerCase.replaceAll("[^a-z]", "");
  }
  
}