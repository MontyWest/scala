package playfair

class Coder(val keyword: String) {

  
  
}

object Coder {
  
  def removePuncutation(str: String): String = {
    str.toLowerCase.replaceAll("[^a-z]", "");
  }
  
}