package ppl.dsl.optiql.baseline.tests.simple;


object Person {

  def SampleData() = List[Person](
    Person(FirstName = "Troy",   LastName = "Magennis",  State = "WA" ),
    Person(FirstName = "Janet",  LastName = "Doherty",   State = "MA" ),
    Person(FirstName = "James",  LastName = "Wann",      State = "CA" ),
    Person(FirstName = "Tara",   LastName = "Wann",      State = "WA" )
  )

}

case class Person (
  FirstName: String,
  LastName: String,
  State: String
)


