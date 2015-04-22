package playfair

class Cipher(val square: Array[Array[Char]]) {

  val cache = collection.mutable.Map[Char, (Int, Int)]()
  
  def encodePair(pair: (Char, Char)): (Char, Char) = transformPair(pair, +1)
  def decodePair(pair: (Char, Char)): (Char, Char) = transformPair(pair, -1)
  
  /***
   * Takes a pair of characters and returns the corresponding pair using the playfair cipher rules
   * dir should be +1 (encoding) or -1 (decoding)
   */
  private def transformPair(pair: (Char, Char), dir: Int): (Char, Char) = {
    val coords = (this.getCharCoordinate(pair._1), this.getCharCoordinate(pair._2))
    
    coords match {
      case (( p , q ), ( r , s )) if p == r => (square(p)(mod5(q+dir)), square(p)(mod5(s+dir)))
      case (( p , q ), ( r , s )) if q == s => (square(mod5(p+dir))(q), square(mod5(r+dir))(q))
      case (( p , q ), ( r , s ))           => (square(p)(s), square(r)(q))
    }
  }
  
  private def mod5(x: Int): Int = (((x % 5) + 5 ) % 5)
  
  
  /***
   * Finds the coordinates of a character in the cipher square
   * This method is memoised
   */
  def getCharCoordinate(char: Char): (Int, Int) = {
    cache get char match {
      case Some(coord) => coord
      case None => {
        val yc = for{
          (arr, i) <- square.zipWithIndex
          (c, j) <- arr.zipWithIndex
          if( c == char )
        } yield (i, j)
        val coord = yc(0)
        cache(char) = coord
        coord
      }
    }
  }
}

object Cipher {
  val chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  
  /**
   * Returns a cipher for the given keyword
   */
  def apply(keyword: String) = {
    val keywordSafe: String = Coder.removePunctuation(keyword).replace("j", "i");
    new Cipher((keywordSafe.toList ++ chars).distinct
                                            .toArray
                                            .grouped(5)
                                            .toArray)
  }
  
}