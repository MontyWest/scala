package test.playfair

import org.scalatest.FlatSpec
import playfair.TextPairs

class TextPairsSpec extends FlatSpec {

  "toString" should "format as rows of 10 words, 5 letters per word, each separated by a space" in {
    val tp: TextPairs = new TextPairs(List(
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),
                         ('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c'),('a', 'c')))
    
    val str: String = tp.toString();
    println(str);
    val lineArr: Array[String] = str.split("\n");
    val wordArrs: Array[Array[String]] = lineArr.map { str => str.split(" ") }
    assert(lineArr.length == 3);
    assert(wordArrs(0).length == 10);
    assert(wordArrs(1).length == 10);
    assert(wordArrs(2).length == 6);
    for (word <- wordArrs.flatten) {
      assert(word.length() == 5);
    }
  }
  
  "fromCiphertext" should "take string of lowercase letters and build ciphertext" in {
      val tp: TextPairs = TextPairs.fromCiphertext("fdjsfbjdkgbrjfhiodbjkfdbgfdgjfldgjfkdbgfndbfjdkr")
      println(tp);
      assert(tp.length == 24)
  }
  
  "fromPlaintext" should "apply playfair variant rules in making text pairs" in {
    
    val pt: String = "jimmypossessesjoopxx"
    val expected: TextPairs = new TextPairs(List(('i', 'x'), ('i', 'm'), ('m', 'y'), ('p', 'o'), ('s', 'x'), ('s', 'e'), ('s', 'x'), ('s', 'e'), ('s', 'i'), ('o', 'x'), ('o', 'p'), ('x', 'q'), ('x', 'z')))
    
    val tp: TextPairs = TextPairs.fromPlaintext(pt);
    
    assert(tp.length == expected.length)
    for(i <- 0 until expected.length) {
      assert(expected(i) == tp(i));
    }
  }
  
}