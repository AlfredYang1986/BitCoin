package module.othernews

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

object OtherNews {
     def addOtherNews(data : JsValue) : JsValue = {
         val title = (data \ "other_id").asOpt[String].get
         val article = (data \ "other_news").asOpt[String].get
         val time_span = new Date().getTime
         
         val builder = MongoDBObject.newBuilder
         builder += "other_id" -> Sercurity.md5Hash(title + time_span)
         builder += "title" -> title
         builder += "othernews" -> article
         builder += "date" -> time_span
         
         _data_connection.getCollection("othernews") += builder.result
        
         toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
     }
    
     def queryOtherNews(data : JsValue) : JsValue = {
         val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
         val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
         toJson(Map("status" -> toJson("ok"), "result" -> toJson(
             ((from db() in ("othernews")).selectSkipTop(skip)(take)("date") { x =>
                 x.getAs[String]("title").get
             }).toList
         )))
     }
     
     def queryOtherNewsDetail(data : JsValue) : JsValue = {
         val report_id = (data \ "other_id").asOpt[String].get
         
         ((from db() in ("othernews") where ("other_id" -> report_id)).selectTop(1)("date")(x => x)).toList match {
           case Nil => ErrorCode.errorToJson("report not exist")
           case head :: Nil => toJson(Map("status" -> toJson("ok"), 
                                           "result" -> toJson(Map("title" -> toJson(head.getAs[String]("title").get),
                                                                   "othernews" -> toJson(head.getAs[String]("othernews").get)))))
         }
     } 
}