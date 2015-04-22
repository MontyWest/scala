package test.playfair

import org.scalatest.FlatSpec
import playfair.Coder

class CoderSpec extends FlatSpec{

  "removePunctuation" should "make string lower case and remove punctuation" in {
    val preStr: String = "When, I. ;Went* forth    with 'Harry'\n I (saw)"
    
    assert("wheniwentforthwithharryisaw" == Coder.removePunctuation(preStr))
  }
  
  "decode" should "apply cipher square to ciphertext to produce (partial) plaintext" in {
    //TODO add Mocking on TextPairs and Cipher
    val coder: Coder =  Coder("PennsylvaniaJ")
    
    val ct: String = "gsvqr enkis ynisy nigqw knsxz t"
    val expected: String = "iximm yposx sesxs esiox opxqx z"
    
    assert(expected == coder.decode(ct))
    
  }
  
  "encode" should "apply cipher square to plaintext and produce ciphertext" in {
    //TODO add Mocking on TextPairs and Cipher
    val coder: Coder =  Coder("PennsylvaniaJ")
    
    val pt: String = "Jimmy possesses JOOP! ~xx~"
    val expected: String = "gsvqr enkis ynisy nigqw knsxz t"

    assert(expected == coder.encode(pt))
  }
  
  "encode" should "be inverse of decode, after removing x's, q's and z's and swapping j's for i's" in {
    val coder: Coder =  Coder("thequickbrownfoxjumpsoverthelazydog")

    val pt: String = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
    
    println(coder.decode(coder.encode(pt)))
    
    val adjEDpt = Coder.removePunctuation(coder.decode(coder.encode(pt))).replace("x", "").replace("q", "").replace("z", "").replace("j", "i")
    val adjpt = Coder.removePunctuation(pt).replace("x", "").replace("q", "").replace("z", "").replace("j", "i")
    
    assert(adjpt == adjEDpt)
  }
  
}