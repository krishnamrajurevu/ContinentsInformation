package models


import models.Country.countries

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class City(cityId : Int, cityName : String, countryId : Int)

object  City{

  private var cities=new ListBuffer[City]()

  def getAllCityOfCountry(countryId : Int) : ListBuffer[City]={
    val cityOfCountry :ListBuffer[City] =for(city <- cities if city.countryId==countryId) yield city
    cityOfCountry

  }

  def createCity(cityName: String, countryId: Int): Boolean = {
    val listOfCities = cities.find((city)=>city.cityName==cityName && city.countryId==countryId)
    listOfCities.isDefined match {
      case true => false
      case false => {
        var maxConId =if(cities.isEmpty) 1 else cities.map(_.cityId).reduceLeft(_ max _) + 1
        cities += (City(maxConId, cityName,countryId))
        true
      }
    }
  }

  def getAllCitiesByContinent(continentName: String): ListBuffer[String] = {
    val allContinents=Continent.getAllContinents()
    val continentIds=allContinents.filter(_.continentName==continentName).map(_.continentId)
    val continentId=if(continentIds.isEmpty) 0 else continentIds(0)
    val allCountries=CountryService.getAllCountriesByContinent(continentId)
    val reqCounIds=allCountries.map(_.countryId)
    val allCities=cities.filter(city => reqCounIds.contains(city.countryId)).map(_.cityName)
    allCities
  }

  def getGroupCities(): Map[Char, Seq[String]] = {
    cities.toSeq.map(_.cityName).groupBy(_.charAt(0))
  }

  def removeCitiesOfCountry(countryIds: List[Int]): Unit = {
    cities=cities.filter(city => !countryIds.contains(city.countryId))
  }

  def removeCitiesOfCountry(countryId: Int): Unit = {
    cities.find(_.countryId==countryId) match {
      case Some(value) => cities = cities.filter(_.countryId != countryId)
      case None => "Nothing to do"
    }
  }

  def removeCity(cityId: Int): String = {
    cities.find(_.cityId==cityId) match {
      case Some(city) => {cities=cities.filter(_.cityId!=cityId)
      "City successfully deleted"
      }
      case None => "City not found"
    }

   // cities=cities.dropWhile(_.cityId==cityId)
  }


}
