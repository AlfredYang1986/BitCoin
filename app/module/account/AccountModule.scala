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

object AccountModule {
 
    def DB2JsValue(x : MongoDBObject) = 
        toJson(Map("total" -> toJson(x.getAs[Float]("total").get), 
                  "btc" -> toJson(x.getAs[Float]("btc").get), 
                  "ltc" -> toJson(x.getAs[Float]("ltc").get),
                  "user_id" -> toJson(x.getAs[String]("user_id").get)
                  ))
  
    def noneFunc = () => ErrorCode.errorToJson("email not exist")
    def defaultErrorFunc = ErrorCode.errorToJson("email not exist")
    def enumUserAccunt(user_id : String, data : JsValue)(successFunc : MongoDBObject => JsValue)(noneFunc : () => JsValue = noneFunc)(implicit errorFunc : JsValue = defaultErrorFunc) : JsValue = {
        (from db() in "accounts" where ("user_id" -> user_id) select (x => x)).toList match {
          case Nil => errorFunc
          case head :: Nil => successFunc(head)  
          case _ => noneFunc()
        }
    }
  
    def queryAccount(user_id : String, data : JsValue) : JsValue = {
    
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        enumUserAccunt(user_id, data)  { head => 
            DB2JsValue(head)
        } { () => 
              val builder = MongoDBObject.newBuilder
              builder += "total" -> total
              builder += "btc" -> 0
              builder += "ltc" -> 0
              builder += "user_id" -> user_id
              
              _data_connection.getCollection("accounts") += builder.result
              DB2JsValue(builder.result)
       }
    }
    
    def pushMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        enumUserAccunt(user_id, data)  { head => 
            head += "total" -> (head.getAs[Float]("total").get + total.asInstanceOf[Float]).asInstanceOf[Number]
            _data_connection.getCollection("accounts").update(DBObject("user_id" -> user_id), head)
            DB2JsValue(head)
        } (noneFunc) 
    }

    def popMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        enumUserAccunt(user_id, data)  { head => 
            val tmp = head.getAs[Float]("total").get - total.asInstanceOf[Float]
            
            if (tmp < 0) ErrorCode.errorToJson("not have enough money") 
            else {
                head += "total" -> tmp.asInstanceOf[Number]
                _data_connection.getCollection("accounts").update(DBObject("user_id" -> user_id), head)
                DB2JsValue(head)
            }
        } (noneFunc) 
    }
}