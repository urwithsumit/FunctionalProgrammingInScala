package com.practice.functional.prgm.scala

/**
  * Example from video series.
  *
  * I see this example as an approach to create function chaining in a modular way.
  * If we have a set of rules, this approach can be used to create rule chains, and as per requirement,
  * these rule chains can be applied to a ls of data.
  *
  * This is very close to Chain of Responsibility Pattern.
  *
  */
object FunctionsInList extends App {

  val InputString = List("Lego", "is", "an", "addiction", "and", "expensive")

  def nonModularApproach(inputString: List[String]) = {
    println(inputString.mkString(","))
    val Output = inputString.filter(_.contains("a")).filter(_.size % 2 == 0)

    println(Output.mkString(","))
  }

  def modularFunctionalApproach(inputString: List[String]) = {
    println(inputString.mkString(","))

    def isEvenSize(str: String): Boolean = str.size % 2 == 0

    def containsA(str: String): Boolean = str.contains("a")

    val FunctionList: List[String => Boolean] = List(isEvenSize , containsA)

    val Output = FunctionList.foldLeft(inputString)((acc, f) => acc.filter(f))

    println(Output.mkString(","))
  }

  nonModularApproach(InputString)

  modularFunctionalApproach(InputString)


}
