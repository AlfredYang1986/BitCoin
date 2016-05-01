package module.account

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity

object AccountModule {
    
    def enumUserAccunt(user_id : String, data : JsValue) : Option[JsValue] = {
        (from db() in "accounts" where ("user_id" -> user_id) select (x => x)).toList match {
          case Nil => None
          case head :: Nil => {
              Some(toJson(Map("total" -> toJson(head.getAs[Float]("total").get), 
                  "btc" -> toJson(head.getAs[Float]("btc").get), 
                  "ltc" -> toJson(head.getAs[Float]("ltc").get),
                  "user_id" -> toJson(user_id)
                  )))
          }
          case _ => None
        }
    }
  
    def queryAccount(user_id : String, data : JsValue) : JsValue = {
    
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
      
        this.enumUserAccunt(user_id, data) match {
          case Some(x) => x
          case None => {
              val builder = MongoDBObject.newBuilder
              builder += "total" -> total
              builder += "btc" -> 0
              builder += "ltc" -> 0
              builder += "user_id" -> user_id
              
              _data_connection.getCollection("accounts") += builder.result
              
              val n = total.asInstanceOf[Number].floatValue
              toJson(Map("total" -> toJson(n),
                  "btc" -> toJson(0),
                  "ltc" -> toJson(0),
                  "user_id" -> toJson(user_id)
                  ))
          }
        }
    }
}