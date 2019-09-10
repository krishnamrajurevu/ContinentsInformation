package models

import scala.collection.mutable.ListBuffer


case class Country(countryId : Int,countryName : String,continentId : Int)

object Country{

  private var countries=new ListBuffer[Country]()

  def getAllCountries(): ListBuffer[Country] = countries

  def getAllCountriesOfContinent(continentId : Int) : ListBuffer[Country]={
    val countriesOfContinent :ListBuffer[Country] =for(country <- countries if country.continentId==continentId) yield country
    countriesOfContinent
  }

  def addCountry(countryName: String, continentId: Int): Boolean = {
    val listOfCountries = countries.find((country)=>country.countryName==countryName && country.continentId==continentId )
    listOfCountries.isDefined match {
      case true => false
      case false => {
        var maxConId =if(countries.isEmpty) 1 else countries.map(_.countryId).reduceLeft(_ max _) + 1
        countries += (Country(maxConId, countryName,continentId))
        true
      }
    }
  }

  def getContinent(countryName: String): String = {
    val reqContinentIds=countries.filter(_.countryName==countryName).map(_.continentId)
    val continentId=if(reqContinentIds.isEmpty) 0 else reqContinentIds(0)
    val continents=Continent.getAllContinents()
    val reqContinentNames=continents.filter(_.continentId==continentId).map(_.continentName)
    val continentName=if(reqContinentNames.isEmpty) s"Country $countryName not existed" else s"Continent of $countryName is "+reqContinentNames(0)
    println("continentName ::"+continentName)
    continentName
  }

  def countriesInContinent(firstCountry: String, secondCountry: String) : String = {
   val continentIds= countries.filter(country => country.countryName==firstCountry || country.countryName==secondCountry ).map(_.continentId)
    if(continentIds.isEmpty){
      s"Countries $firstCountry and $secondCountry are not existed in any continent"
    }else if(continentIds.size==1){
      s"One of country $firstCountry or $secondCountry not existed in any continent"
    }else if(continentIds.size==2){
      if(continentIds(0)==continentIds(1)) s"Countries $firstCountry and $secondCountry are in same continent" else s"Countries $firstCountry and $secondCountry are not in same continent"
    }else{
      "Something went wrong"
    }
  }

  def removeCountryByContinent(continentId: Int): Unit = {
    countries.find(_.continentId == continentId) match {
      case Some(value) =>{
        val countryIds = countries.filter(_.continentId==continentId).map(_.countryId).toList
        CityService.removeCitiesOfCountry(countryIds)
        countries = countries.filter(_.continentId != continentId)
      }
      case None => "Nothing to do"
    }
  }

  def removeCountry(countryId: Int): String = {

    countries.find(_.countryId==countryId) match {
      case Some(value) => {
        CityService.removeCitiesOfCountry(countryId)
        countries = countries.filter(_.countryId != countryId)
        "Country and all cities of country  successfully deleted"
      }
      case None => "Country not found"
    }
  }
}


