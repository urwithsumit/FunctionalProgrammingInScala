package com.scalaprogramming.competitive


object StudentAttendence {
  def main(args: Array[String]) = {
    assert(checkRecord("PPALLP") == true)
    assert(checkRecord("PPALLL") == false)
    assert(checkRecord("PPALAL") == false)
    assert(checkRecord("PPALLP") == true)
  }


  def checkRecord(s: String): Boolean = {

   if( s.filter(_ == 'A').size > 1 || s.contains("LLL")) false else true

  }

}
