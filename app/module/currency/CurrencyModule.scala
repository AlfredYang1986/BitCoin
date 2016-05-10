package module.currency

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
import module.auth.AuthModule

object CurrencyModule {
    
    def DB2JsValue(obj : MongoDBObject) : JsValue = {
        toJson(Map("tag" -> toJson(obj.getAs[String]("tag").get),
                   "name" -> toJson(obj.getAs[String]("name").get),
                   "commition" -> toJson(Math.round(obj.getAs[Number]("commition").get.floatValue * 100) / 100.0),
                   "withdraw_limit" -> toJson(obj.getAs[Number]("withdraw_limit").get.intValue)
                   ))
    }
   
    def createCurrencyNode(data : JsValue) : MongoDBObject = {
        val tag = (data \ "tag").asOpt[String].map (x => x).getOrElse("")
        val name = (data \ "name").asOpt[String].map (x => x).getOrElse("")
        val commition = (data \ "commition").asOpt[Float].map (x => x).getOrElse(0.0)
        val limit = (data \ "limit").asOpt[Int].map (x => x).getOrElse(0)
        
        val builder = MongoDBObject.newBuilder
        builder += "tag" -> tag
        builder += "name" -> name
        builder += "commition" -> commition
        builder += "withdraw_limit" -> limit
        
        builder.result
    }
 
    
    def createCurrency(user_id : String, data : JsValue) : JsValue = {
        if (AuthModule.adminAuthCheck(user_id)) {
            val tag = (data \ "tag").asOpt[String].map (x => x).getOrElse("")
            val name = (data \ "name").asOpt[String].map (x => x).getOrElse("")
            val commition = (data \ "commition").asOpt[Float].map (x => x).getOrElse(0.0)
            val limit = (data \ "limit").asOpt[Int].map (x => x).getOrElse(0)
            
            if (tag == "" || name == "" || commition == 0.0 || limit == 0) ErrorCode.errorToJson("wrong input")
            else {
                (from db() in "currency" where ("tag" -> tag) select (x => x)).toList match {
                  case Nil => {
                      val result = createCurrencyNode(data)
                      _data_connection.getCollection("currency") += result
                      toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(result))))
                  }
                  case head :: Nil => ErrorCode.errorToJson("currency is exist")
                  case _ => ErrorCode.errorToJson("currency is exist")
                }
            }
        } else ErrorCode.errorToJson("not have enough mana")
    }
    
    def updateCurrency(user_id : String, data : JsValue) : JsValue = {
        if (AuthModule.adminAuthCheck(user_id)) {
            val tag = (data \ "tag").asOpt[String].map (x => x).getOrElse("")
            val name = (data \ "name").asOpt[String].map (x => x).getOrElse("")
            val commition = (data \ "commition").asOpt[Float].map (x => x).getOrElse(0.0)
            val limit = (data \ "limit").asOpt[Int].map (x => x).getOrElse(0)
           
            if (tag == "" || name == "" || commition == 0.0 || limit == 0) ErrorCode.errorToJson("wrong input")
            else {
                (from db() in "currency" where ("tag" -> tag) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("currency not exist")
                  case head :: Nil => {
                      head += "name" -> name
                      head += "commition" -> commition.asInstanceOf[Number]
                      head += "withdraw_limit" -> limit.asInstanceOf[Number]
                      _data_connection.getCollection("currency").update(DBObject("tag" -> tag), head)
                      toJson(Map("status" -> "ok", "result" -> "update currency success"))
                  }
                  case _ => ErrorCode.errorToJson("currency not exist")
                }
            }
        } else ErrorCode.errorToJson("not have enough mana")
    }
  
    def currency : JsValue = {
        def currencyImpl : List[JsValue] = {
            ((from db() in "currency") select (x => x)).toList match {
              case Nil => {
                  val result = createCurrencyNode(toJson(Map("tag" -> toJson("CNY"), 
                                                             "name" -> toJson("china yuan"), 
                                                             "commition" -> toJson(0.01), 
                                                             "limit" -> toJson(50000))))
                  _data_connection.getCollection("currency") += result
                  currencyImpl
              }
              case lst : List[MongoDBObject] => {
                  lst map (DB2JsValue(_))
              }
            }
        }
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(currencyImpl)))
    }
}