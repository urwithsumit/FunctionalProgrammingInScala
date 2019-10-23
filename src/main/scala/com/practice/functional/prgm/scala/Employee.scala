package com.practice.functional.prgm.scala

import scala.beans.BeanProperty

/**
  * Learning example from video series.
  *
  * - Using BeanProperty in Class constructor
  * - Using require to check mandatory parameters to the constructor
  * - Using Auxilary Constructor. An auxilary constructor will have less parameter than class constructor.
  * - Class constructor will have maximum parameters.
  * - Aux constructor are define as this()
  *
  *
  * @param firstNm
  * @param lastNm
  */
class Employee(@BeanProperty val firstNm: String, @BeanProperty val lastNm: String) {

  def this() = this("_Scala_", "_Pgm_")

  def copy(firstNm: String = this.firstNm, lastNm: String = this.lastNm) = new Employee(firstNm, lastNm)

  require(firstNm.nonEmpty, "First Name cannot be empty")
  require(lastNm.nonEmpty, "Last Name cannot be empty")

}

object Employee extends App {

  val DefaultEmp = new Employee()

  val Emp = new Employee("Peter", "Nolan")

  val CopyEmp = Emp.copy(firstNm = "Christopher")

  try {
    val CopyEmpErr = Emp.copy(firstNm = "")
    println(s"Copy Emp: ${CopyEmpErr.firstNm} ${CopyEmpErr.lastNm}")
  } catch {
    case ie: IllegalArgumentException => println(ie.getMessage)
  }


  println(s"Default Emp: ${DefaultEmp.firstNm} ${DefaultEmp.lastNm}")

  println(s"Emp: ${Emp.firstNm} ${Emp.lastNm}")


}
