package playfair

class Cipher(val square: Array[Array[Char]]) {

  def transformPair(pair: (Char, Char)): (Char, Char) = {
    val coords = (this.getCharCoordinate(pair._1), this.getCharCoordinate(pair._2))
    
    coords match {
      case (( p , q ), ( r , s )) if p == r => (square(p)((q+1) % 5), square(p)((s+1) % 5))
      case (( p , q ), ( r , s )) if q == s => (square((p+1) % 5)(q), square((r+1) % 5)(q))
      case (( p , q ), ( r , s ))           => (square(p)(s), square(r)(q))
    }
  }
  
  def getCharCoordinate(char: Char): (Int, Int) = {
    val coord = for{
      (arr, i) <- square.zipWithIndex
      (c, j) <- arr.zipWithIndex
      if( c == char )
    } yield (i, j)
    println(coord(0))
    coord(0);
  }
}

object Cipher {
  val chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  def apply(keyword: String) = {
    val keywordSafe: String = Coder.removePunctuation(keyword).replace("j", "i");
    new Cipher((keywordSafe.toList ++ chars).distinct
                                            .toArray
                                            .grouped(5)
                                            .toArray)
  }
  
}