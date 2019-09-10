package models

import scala.collection.mutable.ListBuffer

case class Continent(continentId: Int, continentName: String)

object Continent {
  private val continents = new ListBuffer[Continent]()

  def getAllContinents(): ListBuffer[Continent] = continents

  def addContinent(continentName: String) = {
    continents.find(_.continentName == continentName) match {
      case Some(_) => None
      case None =>
        val maxConId = continents.maxByOption(_.continentId).map(_.continentId + 1).getOrElse(1)
        val continent = Continent(maxConId, continentName)
        continents += continent
        Some(continent)
    }

  }

  def removeContinent(continentId: Int): String = {
    continents.find(_.continentId == continentId).fold("Nothing to do") { value =>
      CountryService.removeCountryByContinent(continentId)
      continents.filterInPlace(_.continentId != continentId)
      "Continent,countries,cities of continents successfully deleted"
    }
  }
}
