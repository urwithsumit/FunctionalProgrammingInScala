package com.scalaprogramming.fun


object PhoneBook {

  case class Person(name: String, age: Int)

  class AddressBook(a: Person*) {
    private val people: List[Person] = a.toList

    def toXHTML =
      <table cellpadding="2" cellspacing="0">
        <tr>
          <th>Name</th>
          <th>Age</th>
        </tr>

        {for (p <- people) yield {
        <tr>
          <td> {p.name} </td>
          <td> {p.age} </td>
        </tr>
      }}

      </table>
  }

  val header = <head>
  <title>{"My Address Book"}</title>
    <style type ="text/css">
    {
      """
        |table{ border-right: 1px solid #cccccc;}
        |th { background-color: #cccccc; }
        |td { border-left: 1px solid #acacac; }
        |td { border-bottom: 1px solid #acacac; }
        |""".stripMargin
      }
    </style>
  </head>

  val people = new AddressBook(
    Person("Tom", 36),
    Person("Harry", 50)
  )

  val page = <html>
    {header}
    <body>
      {people.toXHTML}
    </body>
  </html>

  def main(args: Array[String]) = {
    println(page)
  }

}
