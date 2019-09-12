package models


import models.Country.countries

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class City(cityId: Int, cityName: String, countryId: Int)

object City {

  private val cities = new ListBuffer[City]()

  def getAllCityOfCountry(countryId: Int): List[City] = {
    for (city <- cities.toList if city.countryId == countryId) yield city
  }

  def createCity(cityName: String, countryId: Int): Option[String] = {
    CountryService.getAllCountries().find(_.countryId == countryId) match {
      case Some(value) =>
        cities.find(_.cityName != cityName).fold[Option[String]](None) { v =>
          cities += City(cities.maxByOption(_.cityId).map(_.cityId + 1).getOrElse(1), cityName, countryId)
          Some(s"City $cityName created successfully")
        }
      case None => None
    }
  }

  def getAllCitiesByContinent(continentName: String): Option[List[String]] = {
    val continentIds: List[Int] = Continent.getAllContinents().filter(_.continentName == continentName).map(_.continentId)
    continentIds match {
      case ::(head, next) => CountryService.getAllCountriesByContinent(head) match {
        case Some(value) => val countryIds = value.map(_.countryId)
          countryIds match {
            case ::(head, next) => Some(cities.filter(city => countryIds.contains(city.countryId)).map(_.cityName).toList)
            case Nil => None
          }
        case None => None
      }
      case Nil => None
    }
  }

  def getGroupCities(): Map[Char, Seq[String]] = {
    cities.toSeq.map(_.cityName).groupBy(_.charAt(0))
  }

  def removeCitiesOfCountry(countryIds: List[Int]): Option[String] = {
    cities.find(city => countryIds.contains(city.countryId)) match {
      case Some(value) => cities.filterInPlace(city => !countryIds.contains(city.countryId))
        Some("cities")
      case None => None
    }
  }

  def removeCitiesOfCountry(countryId: Int): Option[String] = {
    cities.find(_.countryId == countryId) match {
      case Some(value) => cities.filterInPlace(_.countryId != countryId)
        Some(" and cities of country ")
      case None => None
    }
  }

  def removeCity(cityId: Int): Option[String] = {
    cities.find(_.cityId == cityId) match {
      case Some(city) =>
        cities.filterInPlace(_.cityId != cityId)
        Some("City successfully deleted")
      case None => None
    }
  }


}
