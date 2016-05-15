package module.bank

import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue


import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.auth.AuthModule.adminAuthCheck

object BankModule {
    def updateBankAccount(user_id : String, data : JsValue) : JsValue = {
        if (adminAuthCheck(user_id)) {
            (from db() in "bank" where ("key" -> 0) select (x => x)).toList match {
              case Nil => DB2JsValue(createBankModel(data))
              case head :: Nil => {
                  head += "account" -> (data \ "account").asOpt[String].map (x => x).getOrElse("")
                  _data_connection.getCollection("bank").update(DBObject("key" -> 0), head)
                  DB2JsValue(head)
              }
            }
        } else ErrorCode.errorToJson("not have enough mana")
    }
    
    def queryBankAccount(user_id : String, data : JsValue) : JsValue = 
        (from db() in "bank" where ("key" -> 0) select (x => x)).toList match {
            case Nil => DB2JsValue(createBankModel(data))
            case head :: Nil => DB2JsValue(head)
        }
    
    def createBankModel(data : JsValue) : MongoDBObject = {
        val builder = MongoDBObject.newBuilder
        builder += "account" -> (data \ "account").asOpt[String].map (x => x).getOrElse("")
        builder += "key" -> 0
        
        val result = builder.result
        _data_connection.getCollection("bank") += result
        result
    }
    
    def DB2JsValue(obj : MongoDBObject) : JsValue =
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("account" -> toJson(obj.getAs[String]("account").get)))))
}