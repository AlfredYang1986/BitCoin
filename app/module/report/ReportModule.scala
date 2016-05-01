package module.report

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import java.util.Date

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity._

object ReportModule {
     def addReport(data : JsValue) : JsValue = {
         val title = (data \ "title").asOpt[String].get
         val article = (data \ "art").asOpt[String].get
         val time_span = new Date().getTime
         
         val builder = MongoDBObject.newBuilder
         builder += "report_id" -> Sercurity.md5Hash(title + time_span)
         builder += "title" -> title
         builder += "art" -> article
         builder += "date" -> time_span
         
         _data_connection.getCollection("reports") += builder.result
        
         toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
     }
    
     def queryReports(data : JsValue) : JsValue = {
         val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
         val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
         toJson(Map("status" -> toJson("ok"), "result" -> toJson(
             ((from db() in ("reposts")).selectSkipTop(skip)(take)("date") { x =>
                 x.getAs[String]("title").get
             }).toList
         )))
     }
     
     def queryReportDetail(data : JsValue) : JsValue = {
         val report_id = (data \ "report_id").asOpt[String].get
         
         ((from db() in ("reports") where ("report_id" -> report_id)).selectTop(1)("date")(x => x)).toList match {
           case Nil => ErrorCode.errorToJson("report not exist")
           case head :: Nil => toJson(Map("status" -> toJson("ok"), 
                                           "result" -> toJson(Map("title" -> toJson(head.getAs[String]("title").get),
                                                                   "art" -> toJson(head.getAs[String]("art").get)))))
         }
     }
}