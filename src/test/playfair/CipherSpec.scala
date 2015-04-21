package test.playfair

import org.scalatest.FlatSpec
import playfair.Cipher

class CipherSpec extends FlatSpec {

  "apply" should "convert keyword into cipher square" in {
    
    val ciph: Cipher = Cipher("PennsylvaniaJ")
    val requireSquare: Array[Array[Char]] = Array(
                Array('p', 'e', 'n', 's', 'y'),
                Array('l', 'v', 'a', 'i', 'b'),
                Array('c', 'd', 'f', 'g', 'h'),
                Array('k', 'm', 'o', 'q', 'r'),
                Array('t', 'u', 'w', 'x', 'z'))            
    val requireString = "pensylvaibcdfghkmoqrtuwxz"
    
    assert(ciph.square.flatten.mkString("") == requireString);
    assert(ciph.square.length == 5);
    for (arr <- ciph.square) {
      assert(arr.length == 5)
    }
    assert(ciph.square.deep == requireSquare.deep)
  }
  
  "getCharCoordinate" should "return the coordinate of a char in the square" in {
    
    val ciph: Cipher = Cipher("PennsylvaniaJ")

    assert(ciph.getCharCoordinate('a') == (1, 2))
    assert(ciph.getCharCoordinate('x') == (4, 3))
    assert(ciph.getCharCoordinate('b') == (1, 4))
    assert(ciph.getCharCoordinate('k') == (3, 0))
    assert(ciph.getCharCoordinate('d') == (2, 1))
  }
  
  "transformPair" should "take pair and return opposite corner pair" in {
    
    val ciph: Cipher = Cipher("PennsylvaniaJ")
    
    assert(ciph.transformPair(('p', 'h')) == ('y', 'c'))
    assert(ciph.transformPair(('a', 'f')) == ('f', 'o'))
    assert(ciph.transformPair(('u', 'm')) == ('e', 'u'))
    assert(ciph.transformPair(('v', 's')) == ('i', 'e'))
    assert(ciph.transformPair(('g', 'h')) == ('h', 'c'))
    assert(ciph.transformPair(('u', 'x')) == ('w', 'z'))
    
  }
  
}