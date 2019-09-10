package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CountryService {

  def getAllCountries() : ListBuffer[Country]

  def getAllCountriesByContinent(continentId : Int) : ListBuffer[Country]

  def createCountry(countryName: String, continentId: Int): mutable.Map[String, String]

  def getContinentByCountry(countryName: String) : String

  def checkCountriesInSameContinent(firstCountry: String, firstCountry1: String): String

  def removeCountryByContinent(continentId: Int)

  def removeCountry(countryId: Int) : String
}

object CountryService extends CountryService {

  def getAllCountries() : ListBuffer[Country] = {
    val countries : ListBuffer[Country]=Country.getAllCountries();
    countries
  }


  def getAllCountriesByContinent(continentId : Int) : ListBuffer[Country]={
    Country.getAllCountriesOfContinent(continentId)
  }

  override def createCountry(countryName: String, continentId: Int): mutable.Map[String, String] = {
    var resultMap=mutable.Map[String,String]()
    if(Country.addCountry(countryName,continentId)) {
      resultMap("status")= "success"
      resultMap("message")=s"Country $countryName added successfully"
    }else{
      resultMap("status")= "failed"
      resultMap("message")=s"Country $countryName not added"
    }
    resultMap
  }

  def getContinentByCountry(countryName: String) : String = {
    val continentName=Country.getContinent(countryName)
    continentName
  }


  override def checkCountriesInSameContinent(firstCountry: String, secondCountry: String): String = {
    val message : String=Country.countriesInContinent(firstCountry,secondCountry)
    message
  }

  override def removeCountryByContinent(continentId: Int): Unit = {
    Country.removeCountryByContinent(continentId)
  }

  override def removeCountry(countryId: Int): String = {
    val message : String =Country.removeCountry(countryId)
    message
  }
}
