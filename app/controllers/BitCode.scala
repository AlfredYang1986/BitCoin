package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws._

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import akka.util.Timeout
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import com.ning.http.client.Realm.AuthScheme

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import module.auth._
import controllers.common.requestArgsQuery.{requestGetRequestArgs}
import module.report.ReportModule
import module.bitnews.BitNews
import module.othernews.OtherNews

object BitCode extends Controller {

  def landing = Action {
    Ok(views.html.landing())
  }
 
  def sign = Action {
    Ok(views.html.sign())
  }

  def home = Action { request =>
      val reports = (ReportModule.queryReports(toJson("")) \ "result").asOpt[List[String]].get
      val bitnews = (BitNews.queryBitNews(toJson("")) \ "result").asOpt[List[String]].get
      val othernews = (OtherNews.queryOtherNews(toJson("")) \ "result").asOpt[List[String]].get
      Ok(views.html.home(reports)(bitnews)(othernews)) 
//      if (requestGetRequestArgs(request)(AuthModule.authCheck)) {
//      }
    } 
}