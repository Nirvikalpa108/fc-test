package example

import scala.annotation.tailrec


object RunLengthEncoding extends App {
  println(encode("") == "")
  println(encode("XYZ") == "XYZ")
  println(encode("XYZ"))
  println(encode("AABBBCCCC") == "2A3B4C")
  println(encode("AABBBCCCC"))
  println(encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB") == "12WB12W3B24WB")
  println(encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"))
  println(encode("  hsqq qww  ") == "2 hs2q q2w2 ")
  println(encode("aabbbcccc") == "2a3b4c")
  println(decode("") == "")
  println(decode("XYZ") == "XYZ")
  println(decode("2A3B4C") == "AABBBCCCC")

  def encode(s: String): String = {
    @tailrec
    def loop(rest: List[String], acc: String): String = {
      rest match {
        case head :: tail =>
          val tailOfGivenLetter = tail.span(_ == head)._1
          val tailWithoutGivenLetter = tail.span(_ == head)._2
          val numberOfCharForGivenLetter: Int = tailOfGivenLetter.length + 1

          if (numberOfCharForGivenLetter == 1) loop(tail, acc ++ head)
          else loop(tailWithoutGivenLetter, acc ++ numberOfCharForGivenLetter.toString ++ head)
        case Nil => acc
      }
    }
    loop(s.split("").toList, "")
  }

  // if there is a number, then take the next letter and do multiple
  // Q - how do I tell if it's a number??
  // if there is a letter, just print it
  def decode(s: String): String = ???

}


//  test("decode - string with no single characters") {
//    pending
//    RunLengthEncoding.decode("2A3B4C") should be ("AABBBCCCC")
//  }
//
//  test("decode - single characters with repeated characters") {
//    pending
//    RunLengthEncoding.decode("10WB12W3B24WB") should be ("WWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB")
//  }
//
//  test("decode - multiple whitespace mixed in string") {
//    pending
//    RunLengthEncoding.decode("2 hs2q q2w2 ") should be ("  hsqq qww  ")
//  }
//
//  test("decode - lower case string") {
//    pending
//    RunLengthEncoding.decode("2a3b4c") should be ("aabbbcccc")
//  }
//
//  test("consistency - encode followed by decode gives original string") {
//    pending
//    RunLengthEncoding.decode(RunLengthEncoding.encode("zzz ZZ  zZ")) should be ("zzz ZZ  zZ")
//  }
//}
//
//object Solution extends App {
//  new RunLengthEncodingTest().execute()
//}


