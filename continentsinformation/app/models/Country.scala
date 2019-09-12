package models

import scala.collection.mutable.ListBuffer


case class Country(countryId: Int, countryName: String, continentId: Int)

object Country {

  private val countries = new ListBuffer[Country]()

  def getAllCountries(): List[Country] = countries.toList

  def getAllCountriesOfContinent(continentId: Int): Option[List[Country]] = {
    countries.find(_.continentId == continentId) match {
      case Some(value) => Some(countries.filter(_.continentId == continentId).toList)
      case None => None
    }
  }

  def addCountry(countryName: String, continentId: Int): Option[String] = {
    ContinentService.getAllContinent().find(_.continentId == continentId) match {
      case Some(value) => countries.find(_.countryName == countryName) match {
        case Some(value) => None
        case None => val country = Country(countries.maxByOption(_.countryId).map(_.countryId + 1).getOrElse(1), countryName, continentId)
          countries += country
          Some(s"Country $countryName added successfully")
      }
      case None => None
    }
  }

  def getContinent(countryName: String): Option[String] = {
    countries.filter(_.countryName == countryName).map(_.continentId).toList match {
      case ::(head, next) => Continent.getAllContinents().filter(_.continentId == head).map(_.continentName) match {
        case ::(head, next) => Some(head)
        case Nil => None
      }
      case Nil => None
    }
  }

  def countriesInContinent(firstCountry: String, secondCountry: String): Option[String] = {
    countries.toList.filter(country => country.countryName == firstCountry || country.countryName == secondCountry).map(_.continentId) match {
      case list => if (list.size == 1) Some(s"One of country $firstCountry or $secondCountry not existed in any continent")
      else if (list(0) == list(1)) Some(s"Countries $firstCountry and $secondCountry are in same continent")
      else Some(s"Countries $firstCountry and $secondCountry are not in same continent")
      case Nil => Some(s"Countries $firstCountry and $secondCountry are not existed in any continent")
    }
  }

  def removeCountryByContinent(continentId: Int): Option[String] = {
    countries.find(_.continentId == continentId) match {
      case Some(value) => {
        val countryIds = countries.filter(_.continentId == continentId).map(_.countryId).toList
        val message: Option[String] = countryIds match {
          case ::(head, next) => CityService.removeCitiesOfCountry(countryIds)
          case Nil => None
        }
        countries.filterInPlace(_.continentId != continentId)
        message match {
          case Some(value) => Some("countries and " + value)
          case None => Some("countries ")
        }

      }
      case None => None
    }
  }

  def removeCountry(countryId: Int): Option[String] = {

    countries.find(_.countryId == countryId) match {
      case Some(value) => {
        val message = CityService.removeCitiesOfCountry(countryId) match {
          case Some(value) => value
          case None => ""
        }
        countries.filterInPlace(_.countryId != countryId)
        Some(s"Country ${message} successfully deleted")
      }
      case None => None
    }
  }
}


