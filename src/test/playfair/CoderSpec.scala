package test.playfair

import org.scalatest.FlatSpec
import playfair.Coder

class CoderSpec extends FlatSpec{

  "removePunctuation" should "make string lower case and remove punctuation" in {
    val preStr: String = "When, I. ;Went* forth    with 'Harry'\n I (saw)"
    
    assert("wheniwentforthwithharryisaw" == Coder.removePunctuation(preStr))
  }
  
}