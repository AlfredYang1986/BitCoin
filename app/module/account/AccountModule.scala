package module.account

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.concurrent._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.auth.AuthModule

object AccountModule {

    def createAccount(total : Number, user_id : String) : MongoDBObject = {
        val builder = MongoDBObject.newBuilder
              builder += "total" -> total
              builder += "btc" -> 0
              builder += "ltc" -> 0
              builder += "user_id" -> user_id
             
              _data_connection.getCollection("accounts") += builder.result
              builder.result
    }
  
    def DB2JsValue(x : MongoDBObject) : JsValue =
        toJson(Map("total" -> toJson(x.getAs[Number]("total").get.floatValue),
                  "btc" -> toJson(x.getAs[Number]("btc").get.floatValue), 
                  "ltc" -> toJson(x.getAs[Number]("ltc").get.floatValue),
                  "user_id" -> toJson(x.getAs[String]("user_id").get)
                  ))
  
    def noneFunc = () => ErrorCode.errorToJson("email not exist")
    def defaultErrorFunc = ErrorCode.errorToJson("email not exist")
    def enumUserAccunt(user_id : String, data : JsValue)(successFunc : MongoDBObject => JsValue)(noneFunc : () => JsValue = noneFunc)(implicit errorFunc : JsValue = defaultErrorFunc) : JsValue = {
        (from db() in "accounts" where ("user_id" -> user_id) select (x => x)).toList match {
          case Nil => noneFunc()
          case head :: Nil => successFunc(head)  
          case _ => errorFunc
        }
    }
  
    def queryAccount(user_id : String, data : JsValue) : JsValue = {
   
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        enumUserAccunt(user_id, data)  { head => 
            DB2JsValue(head)
        } { () => 
            DB2JsValue(createAccount(total.asInstanceOf[Number], user_id))
       }
    }
    
    def pushMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0)
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        if (AuthModule.adminAuthCheck(user_id)) {
            enumUserAccunt(owner_id, data)  { head => 
                head += "total" -> (head.getAs[Number]("total").get.floatValue + total.asInstanceOf[Float]).asInstanceOf[Number]
                _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                DB2JsValue(head)
            } (noneFunc) 
        } else DB2JsValue(createAccount(total.asInstanceOf[Number], owner_id))
    }

    def popMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        if (AuthModule.adminAuthCheck(user_id)) {
            enumUserAccunt(owner_id, data)  { head => 
                val tmp = head.getAs[Number]("total").get.floatValue - total.asInstanceOf[Float]
                if (tmp < 0) ErrorCode.errorToJson("not have enough money") 
                else {
                    head += "total" -> tmp.asInstanceOf[Number]
                    _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                    DB2JsValue(head)
                }
            } (noneFunc) 
        } else ErrorCode.errorToJson("not have enough mana")
    }
}