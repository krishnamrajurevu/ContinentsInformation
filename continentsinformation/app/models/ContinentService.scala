package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait ContinentService {
  def getAllContinent(): ListBuffer[Continent]

  def createContinent(continentName: String): mutable.Map[String, String]

  def removeContinent(continentId: Int): String
}

object ContinentService extends ContinentService {

  override def getAllContinent(): ListBuffer[Continent] = Continent.getAllContinents()

  override def createContinent(continentName: String): mutable.Map[String, String] = {
    var resultMap = mutable.Map[String, String]()
    try {
      Continent.addContinent(continentName) match {
        case Some(continent) =>
          resultMap("status") = "success"
          resultMap("message") = s"Continent $continentName added successfully"
        case None =>
          resultMap("status") = "failed"
          resultMap("message") = s"Continent $continentName not added"
      }
      resultMap
    } catch {
      case e: Exception => {
        resultMap("status") = "failed"
        resultMap("message") = s"Continent $continentName not added"
      }
    }
    resultMap
  }

  override def removeContinent(continentId: Int): String = {
    Continent.removeContinent(continentId)
  }
}


