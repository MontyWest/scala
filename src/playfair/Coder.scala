package playfair

class Coder(val keyword: String) {
  private var optCipher: Option[Cipher] = None;
  val cipher = optCipher.getOrElse(createCipher)
  /**
   * Creates the cipher from the keyword
   */
  def createCipher() {
    optCipher = Some(Cipher(Coder.removePunctuation(keyword)))
  }
  
  def decode(ciphertext: String): String = ???
  
  def encode(plaintext: String): String = ???
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