package ppl.dsl.optiql.baseline.tests.simple

import ppl.dsl.optiql.baseline.util.Date

object Contact {

  def SampleData() = List[Contact] (
    Contact (FirstName = "Barney",     LastName = "Gottshall",     DateOfBirth =  Date(1945,10,19), Phone = "885 983 8858", Email = "bgottshall@aspiring–technology.com", State = "CA" ),
    Contact (FirstName = "Armando",    LastName = "Valdes",        DateOfBirth =  Date(1973,12,9), Phone = "848 553 8487", Email = "val1@aspiring–technology.com", State = "WA" ),
    Contact (FirstName = "Adam",       LastName = "Gauwain",       DateOfBirth =  Date(1959,10,03), Phone = "115 999 1154", Email = "adamg@aspiring–technology.com", State = "AK" ),
    Contact (FirstName = "Jeffery",    LastName = "Deane",         DateOfBirth =  Date(1950,12,16), Phone = "677 602 6774", Email = "jeff.deane@aspiring–technology.com", State = "CA" ),
    Contact (FirstName = "Collin",     LastName = "Zeeman",        DateOfBirth =  Date(1935,02,10), Phone = "603 303 6030", Email = "czeeman@aspiring–technology.com", State = "FL" ),
    Contact (FirstName = "Stewart",    LastName = "Kagel",         DateOfBirth =  Date(1950,02,20), Phone = "546 607 5462", Email = "kagels@aspiring–technology.com", State = "WA" ),
    Contact (FirstName = "Chance",     LastName = "Lard",          DateOfBirth =  Date(1951,10,21), Phone = "278 918 2789", Email = "lard@aspiring–technology.com", State = "WA" ),
    Contact (FirstName = "Blaine",     LastName = "Reifsteck",     DateOfBirth =  Date(1946,05,18), Phone = "715 920 7157", Email = "blaine@aspiring–technology.com", State = "TX" ),
    Contact (FirstName = "Mack",       LastName = "Kamph",         DateOfBirth =  Date(1977,9,17), Phone = "364 202 3644", Email = "mack.kamph@aspiring–technology.com", State = "TX" ),
    Contact (FirstName = "Ariel",      LastName = "Hazelgrove",    DateOfBirth =  Date(1922,05,23), Phone = "165 737 1656", Email = "arielh@aspiring–technology.com", State = "OR" )
  )
}

case class Contact (
  var FirstName: String,
  var LastName: String,
  var DateOfBirth: Date,
  var Phone: String,
  var Email: String,
  var State: String
)

