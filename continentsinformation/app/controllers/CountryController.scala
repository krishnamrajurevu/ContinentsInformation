package controllers

import javax.inject.Inject
import models.{City, CityService, Continent, ContinentService, Country, CountryService}
import play.api.Configuration
import play.api.http.MimeTypes
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request}
import play.api.routing.JavaScriptReverseRouter
import views.html.countries._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
1. Comments for every method
2. Handling exception
3. Hardcode vales in configuration file
 */
class CountryController @Inject()(config: Configuration, cc : ControllerComponents) extends AbstractController(cc){


  def index()=Action{
     Ok(views.html.countries.index.render("Welcome to the world"))
  }

  def getAllContinents(message : String)=Action{
    val continents: ListBuffer[Continent]=ContinentService.getAllContinent
    Ok(continent_list.render(continents,message))
  }

  def addContinent()=Action{
    Ok(add_continent.render("Add continent to world",""))
  }


  def createContinent() =Action{ request =>
    val reqBody =request.body.asFormUrlEncoded
    reqBody.map{ args =>
      val continentName=args("continentName").head
      var result :mutable.Map[String, String]= ContinentService.createContinent(continentName)
      //Ok(Json.stringify(Json.toJson(result)))
      Ok(add_continent.render("Add continent to world",result.head._2))

    }.getOrElse(Ok("parameters not existed"))
  }

  def deleteContinent(continentId : Int)=Action{
    val message : String = ContinentService.removeContinent(continentId)
    Redirect(routes.CountryController.getAllContinents(message))
  }

  def getCountriesOfContinent(continentId : Int,continentName : String,message : String)=Action{
    val countries : ListBuffer[Country]=CountryService.getAllCountriesByContinent(continentId)
    Ok(countries_list.render(continentId,continentName,countries,message))
  }

  def addCountryToContinent(continentId : Int,continentName : String)=Action{
    Ok(add_country.render("Add country",continentId,continentName,""))
  }

  def createCountry(continentId : Int) =Action{ request: Request[AnyContent] =>

    val reqBody=request.body.asFormUrlEncoded
    reqBody.map{ args =>
      val countryName=args("countryName").head
      val continentName=args("continentName").head
      var result :mutable.Map[String, String]= CountryService.createCountry(countryName,continentId)
      //Ok(Json.stringify(Json.toJson(result)))
      Ok(add_country.render("Add country",continentId,continentName,result.head._2))

    }.getOrElse(Ok("Bad request"))
  }

  def deleteCountry(countryId : Int,continentId : Int,continentName : String) = Action{
    val message : String=CountryService.removeCountry(countryId)
    Redirect(routes.CountryController.getCountriesOfContinent(continentId ,continentName ,message))
  }

  def getCitiesOfCountry(countryId : Int,countryName : String,message : String)=Action{
    val cities : ListBuffer[City]=CityService.getAllCitiesByCountry(countryId)
    Ok(cities_list.render(countryId,countryName,cities,message))
  }


  def addCityToCountry(countryId : Int,countryName : String)=Action{
    Ok(add_city.render("Add city",countryId,countryName,""))
  }

  def createCity(countryId : Int)=Action{ request : Request[AnyContent] =>
    val reqBody=request.body.asFormUrlEncoded
    reqBody.map{ args =>
      val cityName=args("cityName").head
      val countryName=args("countryName").head
      var result :mutable.Map[String, String]= CityService.createCity(cityName,countryId)
      //Ok(Json.stringify(Json.toJson(result)))
      Ok(add_city.render("Add city",countryId,countryName,result.head._2))
    }.getOrElse(Ok("Bad request"))

  }

  def deleteCity(cityId : Int,countryId : Int,countryName : String)=Action{
    val message = CityService.removeCity(cityId)
    Redirect(routes.CountryController.getCitiesOfCountry(countryId,countryName,message))
  }

  def getContinentOfCountry()=Action{
    Ok(get_continent_ofcountry.render("Get continent of country",""))
  }

  def getContinent(countryName : String)=Action{
    val continentName=CountryService.getContinentByCountry(countryName)
    Ok(get_continent_ofcountry.render("Get continent of country",continentName))
  }

  def getAllCitiesOfContinent()=Action{
    Ok(get_cities_ofContinent.render("Get all cities of continent",new ListBuffer[String]()))
  }

  def getAllCities(continentName : String) = Action{
    val cities : ListBuffer[String] =CityService.getAllCitiesByContinent(continentName)
    Ok(get_cities_ofContinent.render("Get all cities of continent", cities))
  }

  def checkTwoCountriesLiesInContinent()=Action{
    Ok(countries_liesin_continent.render("Check countries in same continent" ,""))
  }

  def countriesLieInContinent(firstCountry : String , secondCountry : String) =Action{
    val message : String=CountryService.checkCountriesInSameContinent(firstCountry,secondCountry)
    Ok(countries_liesin_continent.render("Check countries in same continent" ,message))
  }

  def addCitiesToCountry()=Action{

    val countries=CountryService.getAllCountries()
    Ok(add_cities.render("Add Cities with Comma",countries,""))
  }

  def createMultipleCities()=Action{ request: Request[AnyContent] =>
    val reqBody=request.body.asFormUrlEncoded
    reqBody.map{ args =>
      val countryId : Int = args("countryId").head.toInt
      val cities : String = args("cities").head
      val citiesList = cities.split(",")
      for(cityName <- citiesList){
        CityService.createCity(cityName,countryId)
      }
      val countries=CountryService.getAllCountries()
      Ok(add_cities.render("Add Cities with Comma",countries,""))
    }.getOrElse(Ok("Bad request"))
  }

  def getGroupCities()=Action{
    Ok(group_cities.render("Grouping cities by Character",List[String](),""))
  }

  def getGroupedCities(cityFirstChar : Char) =Action{
     val groupCities : Map[Char,Seq[String]]=CityService.getCitiesByGroup()
    println("groupCities ::"+groupCities)
    val cities=groupCities.get(cityFirstChar)
    println("cities ::"+cities)
    cities match {
      case Some(cities) => Ok(group_cities.render("Grouping cities by Character",cities.toList,s"List of cities by character $cityFirstChar"))
      case None => Ok(group_cities.render("Grouping cities by Character",List[String](),s"Cities not find by given character $cityFirstChar"))
    }
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes")(
        routes.javascript.CountryController.deleteContinent,
        routes.javascript.CountryController.deleteCountry,
        routes.javascript.CountryController.deleteCity
      )
    ).as(MimeTypes.JAVASCRIPT)
  }
}

