package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait ContinentService {
  def getAllContinent(): List[Continent]

  def createContinent(continentName: String): Option[Continent]

  def removeContinent(continentId: Int): Option[String]
}

object ContinentService extends ContinentService {

  override def getAllContinent(): List[Continent] = Continent.getAllContinents()

  override def createContinent(continentName: String): Option[Continent] = {
    Continent.addContinent(continentName) match {
      case Some(continent) => Some(continent)
      case None => None
    }
  }

  override def removeContinent(continentId: Int): Option[String] = {
    Continent.removeContinent(continentId)
  }
}


