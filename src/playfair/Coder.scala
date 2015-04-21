package playfair

class Coder(val keyword: String) {

  
  
}

object Coder {
  
  def removePunctuation(str: String): String = {
    str.toLowerCase.replaceAll("[^a-z]", "");
  }
  
}