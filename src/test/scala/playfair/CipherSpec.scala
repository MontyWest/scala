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
  
  "encodePair" should "take pair and return opposite corner pair, or +1 inline" in {
    
    val ciph: Cipher = Cipher("PennsylvaniaJ")
    
    assert(ciph.encodePair(('p', 'h')) == ('y', 'c'))
    assert(ciph.encodePair(('a', 'f')) == ('f', 'o'))
    assert(ciph.encodePair(('u', 'm')) == ('e', 'u'))
    assert(ciph.encodePair(('v', 's')) == ('i', 'e'))
    assert(ciph.encodePair(('g', 'h')) == ('h', 'c'))
    assert(ciph.encodePair(('u', 'x')) == ('w', 'z'))
    
  }
  
  "decodePair" should "take pair and return opposite corner pair, or -1 inline" in {
    
    val ciph: Cipher = Cipher("PennsylvaniaJ")
    
    assert(ciph.decodePair(('y', 'c')) == ('p', 'h'))
    assert(ciph.decodePair(('f', 'o')) == ('a', 'f'))
    assert(ciph.decodePair(('e', 'u')) == ('u', 'm'))
    assert(ciph.decodePair(('i', 'e')) == ('v', 's'))
    assert(ciph.decodePair(('h', 'c')) == ('g', 'h'))
    assert(ciph.decodePair(('w', 'z')) == ('u', 'x'))
    
  }
  
  "encodePair" should "be inverse to decodePair" in {
    
    val ciph: Cipher = Cipher("PennsylvaniaJ")
    
    assert(ciph.decodePair(ciph.encodePair(('y', 'c'))) == ('y', 'c'))
    assert(ciph.decodePair(ciph.encodePair(('a', 'f'))) == ('a', 'f'))
    assert(ciph.decodePair(ciph.encodePair(('e', 'u'))) == ('e', 'u'))
    assert(ciph.decodePair(ciph.encodePair(('v', 's'))) == ('v', 's'))
    assert(ciph.decodePair(ciph.encodePair(('g', 'h'))) == ('g', 'h'))
    assert(ciph.decodePair(ciph.encodePair(('w', 'z'))) == ('w', 'z'))
    
  }
  
}