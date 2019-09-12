package models

import scala.collection.mutable.ListBuffer

case class Continent(continentId: Int, continentName: String)

object Continent {
  private val continents = new ListBuffer[Continent]()

  def getAllContinents(): List[Continent] = continents.toList

  def addContinent(continentName: String): Option[Continent] = {
    continents.find(_.continentName == continentName) match {
      case Some(_) => None
      case None => val continent = Continent(continents.maxByOption(_.continentId).map(_.continentId + 1).getOrElse(1), continentName)
        continents += continent
        Some(continent)
    }
  }

  def removeContinent(continentId: Int): Option[String] = {
    continents.find(_.continentId == continentId).fold[Option[String]](None) { value =>
      val result: Option[String] = CountryService.removeCountryByContinent(continentId)
      val message = result match {
        case Some(value) => ", " + value + " of continents"
        case None => ""
      }
      continents.filterInPlace(_.continentId != continentId)
      Some(s"Continent $message successfully deleted")
    }
  }

}
