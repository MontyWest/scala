package test.playfair

import org.scalatest.FlatSpec
import playfair.CipherText

class CipherTextSpec extends FlatSpec {

  "toString" should "format as rows of 10 words, 5 letters per word, each separated by a space" in {
    val ct: CipherText = new CipherText(List(
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
    
    val str: String = ct.toString();
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
  
  "apply" should "take string of lowercase letters and build ciphertext" in {
      val ct: CipherText = CipherText("fdjsfbjdkgbrjfhiodbjkfdbgfdgjfldgjfkdbgfndbfjdkr")
      println(ct);
      assert(ct.message.length == 24)
  }
  
}