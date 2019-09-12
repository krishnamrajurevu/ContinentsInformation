package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CityService {
  def getAllCitiesByCountry(countryId: Int): List[City]

  def createCity(cityName: String, countryId: Int): Option[String]

  def getAllCitiesByContinent(continentName: String): Option[List[String]]

  def getCitiesByGroup(): Map[Char, Seq[String]]

  def removeCitiesOfCountry(countryIds: List[Int]): Option[String]

  def removeCitiesOfCountry(countryId: Int): Option[String]

  def removeCity(cityId: Int): Option[String]
}

object CityService extends CityService {

  override def getAllCitiesByCountry(countryId: Int): List[City] = City.getAllCityOfCountry(countryId)

  override def createCity(cityName: String, countryId: Int): Option[String] = {
    City.createCity(cityName, countryId)
  }

  override def getAllCitiesByContinent(continentName: String): Option[List[String]] = {
    City.getAllCitiesByContinent(continentName)
  }

  override def getCitiesByGroup(): Map[Char, Seq[String]] = {
    City.getGroupCities()

  }

  override def removeCitiesOfCountry(countryIds: List[Int]): Option[String] = {
    City.removeCitiesOfCountry(countryIds)
  }

  override def removeCitiesOfCountry(countryId: Int): Option[String] = {
    City.removeCitiesOfCountry(countryId)
  }

  def removeCity(cityId: Int): Option[String] = {
    City.removeCity(cityId)
  }

}


