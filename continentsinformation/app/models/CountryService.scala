package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CountryService {

  def getAllCountries(): List[Country]

  def getAllCountriesByContinent(continentId: Int): Option[List[Country]]

  def createCountry(countryName: String, continentId: Int): Option[String]

  def getContinentByCountry(countryName: String): Option[String]

  def checkCountriesInSameContinent(firstCountry: String, firstCountry1: String): Option[String]

  def removeCountryByContinent(continentId: Int): Option[String]

  def removeCountry(countryId: Int): Option[String]
}

object CountryService extends CountryService {

  def getAllCountries(): List[Country] = {
    Country.getAllCountries();
  }


  def getAllCountriesByContinent(continentId: Int): Option[List[Country]] = {
    Country.getAllCountriesOfContinent(continentId)
  }

  override def createCountry(countryName: String, continentId: Int): Option[String] = {
    Country.addCountry(countryName, continentId)
  }

  def getContinentByCountry(countryName: String): Option[String] = {
    Country.getContinent(countryName)
  }


  override def checkCountriesInSameContinent(firstCountry: String, secondCountry: String): Option[String] = {
    Country.countriesInContinent(firstCountry, secondCountry)
  }

  override def removeCountryByContinent(continentId: Int): Option[String] = {
    Country.removeCountryByContinent(continentId)
  }

  override def removeCountry(countryId: Int): Option[String] = {
    Country.removeCountry(countryId)
  }
}
