package com.practice.sk

object LongestNonRepeatSubstring {

  def lengthOfLongestSubstring(s: String): Int = {
   val Length =  s.length match {
      case x if x == 0 => 0
      case x if x == 1 => 1
      case x if x == 2 => if(s(0) == s(1)) 1 else 2
      case _ => {
        import scala.collection.mutable.Map
         var initial = 0
         var iSize = 0
         val indexMap: Map[Character, Int] = Map()

         for(i <- 0 until s.length) {

           if(indexMap.contains(s(i))) {
             val Diff = indexMap.get(s(i)).get - initial
             println(s" ${indexMap.get(s(i)).get} - ${initial } = $Diff")
             if(Diff >= 0 && Diff >= iSize) {
               println(s"if: ${s}: ${s.substring(initial, indexMap.get(s(i)).get)}")
               iSize = Diff
               initial = indexMap.get(s(i)).get
             }
           } else {
             if(i - initial >= iSize) {
               println(s" else if: ${s}: ${s.substring(initial, i)}")
               iSize = i - initial
             }
           }

           indexMap.put(s(i), i)
         }

        iSize
          }
        }

    println(Length)
    Length
  }

  def main(args: Array[String]) = {
    assert(lengthOfLongestSubstring("") == 0)
    assert(lengthOfLongestSubstring("q") == 1)
    assert(lengthOfLongestSubstring("wz") == 2)

    assert(lengthOfLongestSubstring("dfrffewwqsdggg") == 5)
    assert(lengthOfLongestSubstring("ww") == 1)
    assert(lengthOfLongestSubstring("bbbbb") == 1)

    assert(lengthOfLongestSubstring("pwwkew") == 3)
    assert(lengthOfLongestSubstring("abcabcbb") == 3)

    assert(lengthOfLongestSubstring("wwc") == 2)
    assert(lengthOfLongestSubstring("dvdf") == 3)


  }


}
