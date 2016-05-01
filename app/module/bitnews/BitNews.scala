package module.bitnews

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

object BitNews {
     def addBitNews(data : JsValue) : JsValue = {
         val title = (data \ "title").asOpt[String].get
         val article = (data \ "bit_news").asOpt[String].get
         val time_span = new Date().getTime
         
         val builder = MongoDBObject.newBuilder
         builder += "bitnews_id" -> Sercurity.md5Hash(title + time_span)
         builder += "title" -> title
         builder += "bitnews" -> article
         builder += "date" -> time_span
         
         _data_connection.getCollection("bitnews") += builder.result
        
         toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
     }
    
     def queryBitNews(data : JsValue) : JsValue = {
         val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
         val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
         toJson(Map("status" -> toJson("ok"), "result" -> toJson(
             ((from db() in ("bitnews")).selectSkipTop(skip)(take)("date") { x =>
                 x.getAs[String]("title").get
             }).toList
         )))
     }
     
     def queryBitNewsDetail(data : JsValue) : JsValue = {
         val report_id = (data \ "bitnews_id").asOpt[String].get
         
         ((from db() in ("bitnews") where ("bitnews_id" -> report_id)).selectTop(1)("date")(x => x)).toList match {
           case Nil => ErrorCode.errorToJson("report not exist")
           case head :: Nil => toJson(Map("status" -> toJson("ok"), 
                                           "result" -> toJson(Map("title" -> toJson(head.getAs[String]("title").get),
                                                                   "bitnews" -> toJson(head.getAs[String]("bitnews").get)))))
         }
     }
}