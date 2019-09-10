package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CityService {
  def getAllCitiesByCountry(countryId : Int):ListBuffer[City]

  def createCity(cityName: String, countryId: Int): mutable.Map[String, String]

  def getAllCitiesByContinent(continentName: String) :  ListBuffer[String]

  def getCitiesByGroup(): Map[Char, Seq[String]]

  def removeCitiesOfCountry(countryIds : List[Int])

  def removeCitiesOfCountry(countryId: Int)

  def removeCity(cityId: Int) : String
}

object CityService extends CityService {

  override def getAllCitiesByCountry(countryId : Int): ListBuffer[City] = City.getAllCityOfCountry(countryId)

  override def createCity(cityName: String, countryId: Int): mutable.Map[String, String] ={
    var resultMap=mutable.Map[String,String]()
    if( City.createCity(cityName,countryId)) {
      resultMap("status")= "success"
      resultMap("message")=s"City $cityName added successfully"
    }else{
      resultMap("status")= "failed"
      resultMap("message")=s"City $cityName not added"
    }
    resultMap
  }

  override def getAllCitiesByContinent(continentName: String):  ListBuffer[String] = {
    City.getAllCitiesByContinent(continentName)
  }

  override def getCitiesByGroup(): Map[Char, Seq[String]] = {
    val groupCities : Map[Char, Seq[String]]= City.getGroupCities()
    groupCities
  }

  override def removeCitiesOfCountry(countryIds: List[Int]): Unit = {
    City.removeCitiesOfCountry(countryIds)
  }

  override def removeCitiesOfCountry(countryId: Int): Unit = {
    City.removeCitiesOfCountry(countryId)
  }

  def removeCity(cityId: Int) : String = {
    City.removeCity(cityId)
  }

}


