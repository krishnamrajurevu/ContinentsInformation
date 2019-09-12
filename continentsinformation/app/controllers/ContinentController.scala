package controllers

import javax.inject.Inject
import models.{City, CityService, Continent, ContinentService, Country, CountryService}
import play.api.Configuration
import play.api.http.MimeTypes
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request}
import play.api.routing.JavaScriptReverseRouter
import views.html.continents._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
1. Comments for every method
2. Handling exception
3. Hardcode vales in configuration file
 */
class ContinentController @Inject()(config: Configuration, cc: ControllerComponents) extends AbstractController(cc) {


  def index() = Action {
    Ok(views.html.continents.index.render(config.get[String]("project_title")))
  }

  def getAllContinents(message: String) = Action {
    val continents: List[Continent] = ContinentService.getAllContinent
    Ok(continent_list.render(continents, message))
  }

  def addContinent() = Action {
    Ok(add_continent.render(config.get[String]("add_continet_title"), ""))
  }


  def createContinent() = Action { request =>
    val reqBody: Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    reqBody.map { args =>
      args.get("continentName") match {
        case Some(continentName) =>
          val message = ContinentService.createContinent(continentName.head) match {
            case Some(value) => s"Continent ${continentName.head} added successfully"
            case None => s"Continent ${continentName.head} already existed"
          }
          Ok(add_continent.render("Add continent to world", message))
        case None => BadRequest("Continent param name not found in request")
      }
    }.getOrElse(BadRequest("Required parameters not found in given request"))
  }

  def deleteContinent(continentId: Int) = Action {
    val result: Option[String] = ContinentService.removeContinent(continentId)
    result match {
      case Some(value) => println("Value ::" + value)
        Redirect(routes.ContinentController.getAllContinents(value))
      case None => BadRequest("Some thing went wrong")
    }

  }

  def getCountriesOfContinent(continentId: Int, continentName: String, message: String) = Action {
    val listOfCountries = CountryService.getAllCountriesByContinent(continentId) match {
      case Some(value) => value
      case None => List[Country]().empty
    }
    Ok(countries_list.render(continentId, continentName, listOfCountries, message))
  }

  def addCountryToContinent(continentId: Int, continentName: String) = Action {
    Ok(add_country.render("Add country", continentId, continentName, ""))
  }

  def createCountry(continentId: Int) = Action { request: Request[AnyContent] =>

    val reqBody: Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    reqBody match {
      case Some(params) =>
        val reqParams: Option[(String, String)] = for {
          countryName <- params.get("countryName")
          continentName <- params.get("continentName")
          tuple = (countryName.head, continentName.head)
        } yield tuple
        reqParams.fold(BadRequest("Mandatory params not found")) { tuple =>
          val message = CountryService.createCountry(tuple._1, continentId) match {
            case Some(value) => value
            case None => "Country not added"
          }
          Ok(add_country.render("Add country", continentId, tuple._2, message))
        }
      case None => BadRequest("Bad Request")
    }
  }

  def deleteCountry(countryId: Int, continentId: Int, continentName: String) = Action {
    val message = CountryService.removeCountry(countryId) match {
      case Some(value) => value
      case None => "Country not deleted"
    }
    Redirect(routes.ContinentController.getCountriesOfContinent(continentId, continentName, message))
  }

  def getCitiesOfCountry(countryId: Int, countryName: String, message: String) = Action {
    val cities: List[City] = CityService.getAllCitiesByCountry(countryId)
    Ok(cities_list.render(countryId, countryName, cities, message))
  }


  def addCityToCountry(countryId: Int, countryName: String) = Action {
    Ok(add_city.render("Add city", countryId, countryName, ""))
  }

  def createCity(countryId: Int) = Action { request: Request[AnyContent] =>
    val reqBody = request.body.asFormUrlEncoded
    reqBody match {
      case Some(formParams) =>
        val result: Option[(String, String)] = for {
          cityName <- formParams.get("cityName")
          countryName <- formParams.get("countryName")
          tuple = (cityName.head, countryName.head)
        } yield tuple
        result.fold(BadRequest("Mandatory params not found")) { tuple =>
          val message = CityService.createCity(tuple._1, countryId) match {
            case Some(value) => value
            case None => s"City ${tuple._1} not created"
          }
          Ok(add_city.render("Add city", countryId, tuple._2, message))
        }
      case None => BadRequest("Bad request")
    }
  }

  def deleteCity(cityId: Int, countryId: Int, countryName: String) = Action {
    val message = CityService.removeCity(cityId) match {
      case Some(value) => value
      case None => "City not deleted"
    }
    Redirect(routes.ContinentController.getCitiesOfCountry(countryId, countryName, message))
  }

  def getContinentOfCountry() = Action {
    Ok(get_continent_ofcountry.render("Get continent of country", ""))
  }

  def getContinent(countryName: String) = Action {
    val continentName = CountryService.getContinentByCountry(countryName) match {
      case Some(value) => s"Continent of $countryName is $value"
      case None => s"Continent of $countryName is not found"
    }
    Ok(get_continent_ofcountry.render("Get continent of country", continentName))
  }

  def getAllCitiesOfContinent() = Action {
    Ok(get_cities_ofContinent.render("Get all cities of continent", List[String]().empty))
  }

  def getAllCities(continentName: String) = Action {
    val cities = CityService.getAllCitiesByContinent(continentName) match {
      case Some(value) => value
      case None => List[String]().empty
    }
    Ok(get_cities_ofContinent.render("Get all cities of continent", cities))
  }

  def checkTwoCountriesLiesInContinent() = Action {
    Ok(countries_liesin_continent.render("Check countries in same continent", ""))
  }

  def countriesLieInContinent(firstCountry: String, secondCountry: String) = Action {
    val message: String = CountryService.checkCountriesInSameContinent(firstCountry, secondCountry) match {
      case Some(value) => value
      case None => "Some thing went wrong"
    }
    Ok(countries_liesin_continent.render("Check countries in same continent", message))
  }

  def addCitiesToCountry() = Action {
    val countries = CountryService.getAllCountries()
    Ok(add_cities.render("Add Cities with Comma", countries, ""))
  }

  def createMultipleCities() = Action { request: Request[AnyContent] =>
    val reqBody = request.body.asFormUrlEncoded
    reqBody match {
      case Some(formParam) =>
        val result: Option[(Int, String)] = for {
          countryId <- formParam.get("countryId")
          cities <- formParam.get("cities")
          tuple = (countryId.head.toInt, cities.head)
        } yield tuple
        result match {
          case Some(tuple) =>
            val citiesList = tuple._2.split(",")
            for (cityName <- citiesList if !cityName.trim.isEmpty) {
              CityService.createCity(cityName.trim(), tuple._1)
            }
            val countries = CountryService.getAllCountries()
            Ok(add_cities.render("Add Cities with Comma", countries, ""))
          case None => BadRequest("Required params not found")
        }
      case None => BadRequest("Bad Request")
    }
  }

  def getGroupCities() = Action {
    Ok(group_cities.render("Grouping cities by Character", List[String](), ""))
  }

  def getGroupedCities(cityFirstChar: Char) = Action {
    val groupCities: Map[Char, Seq[String]] = CityService.getCitiesByGroup()
    val cities = groupCities.get(cityFirstChar)
    val listOfCities = cities match {
      case Some(cities) => cities.toList
      case None => List[String]().empty
    }
    Ok(group_cities.render("Grouping cities by Character", listOfCities, s"List of cities by character $cityFirstChar"))
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes")(
        routes.javascript.ContinentController.deleteContinent,
        routes.javascript.ContinentController.deleteCountry,
        routes.javascript.ContinentController.deleteCity
      )
    ).as(MimeTypes.JAVASCRIPT)
  }
}

