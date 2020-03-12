package com.scalaprogramming.competitive

/** *
  * https://leetcode.com/problems/longest-absolute-file-path/
  *
  */
object LongestAbsFilePath {

  def main(args: Array[String]) = {

    val input1 = "dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext"
    val input2 = "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext"
    val input3 = "a\n\tb\n\t\tc\n\t\t\td\n\t\t\t\te.txt\n\t\t\t\talsdkjf.txt\n\t\tskdjfl.txtlsdkjflsdjflsajdflkjasklfjkasljfklas\n\tlskdjflkajsflj.txt"
    val input4 = "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\t   file2.ext"
    val input5 = "a.txt"
    val input6 = "a"

    assert(lengthLongestPath(input1) == 20)
    assert(lengthLongestPath(input2) == 32)
    assert(lengthLongestPath(input3) == 50)
    assert(lengthLongestPath(input4) == 35)
    assert(lengthLongestPath(input5) == 5)
    assert(lengthLongestPath(input6) == 0)

  }

  def lengthLongestPath(input: String): Int = {
    if (input.isEmpty) 0 else {

      val dirs = input.split("\n")

      def countTabs(idx: Int, symbol: String = "\t") = dirs(idx).replace(symbol, "$").filter(_ == '$').length

      val Output = dirs.foldLeft(Seq[String](), 0: Int)((r, s) => {
        s match {
          case s if s.contains(".") => {
            if (r._2 > 0) {
              val PathBuilderMap = scala.collection.mutable.Map[Int, String]()
              val FileNameTabCount = countTabs(r._2)
              for {
                i <- r._2 - 1 to 0 by -1 //back track from encountered file name
              } yield {
                val ParentDirTabCount = countTabs(i)
                // Parent directory will have Tab Count less than the File Tab Count.
                // If a File Tab count is 4, than the Seq of Parent Tabs till the root dir by back tracking will be 3, 2, 1, 0.
                // Since it is a directory structure, the count of tabs will be unique.
                // When back tracking, immediate parent are encountered first. Once a parent is identified, we ignore other.
                if (ParentDirTabCount < FileNameTabCount && !PathBuilderMap.contains(ParentDirTabCount)) {
                  PathBuilderMap.addOne(ParentDirTabCount -> dirs(i).replaceAll("\t", ""))
                }
              }

              val path = (PathBuilderMap.values.toSeq :+ s.replaceAll("\t", "")).mkString("\\")
              (r._1 :+ path, r._2 + 1)

            } else {
              (r._1 :+ s, r._2 + 1)
            }
          }

          case _ => (r._1, r._2 + 1)
        }
      })

      if (!Output._1.isEmpty) Output._1.map(x => x.length).max else 0
    }
  }

}
