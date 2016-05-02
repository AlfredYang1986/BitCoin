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

    object AccountModuleImpl {
        def createAccount(total : Number, user_id : String) : MongoDBObject = {
            val builder = MongoDBObject.newBuilder
                  builder += "balance" -> total
                  builder += "btc" -> 0
                  builder += "ltc" -> 0
                  builder += "user_id" -> user_id
                 
                  _data_connection.getCollection("accounts") += builder.result
                  builder.result
        }
      
        def DB2JsValue(x : MongoDBObject) : JsValue =
            toJson(Map("balance" -> toJson(x.getAs[Number]("balance").get.floatValue),
                      "btc" -> toJson(x.getAs[Number]("btc").get.floatValue), 
                      "ltc" -> toJson(x.getAs[Number]("ltc").get.floatValue),
                      "user_id" -> toJson(x.getAs[String]("user_id").get)
                      ))
      
        def noneFunc = () => ErrorCode.errorToJson("email not exist") 
        def defaultErrorFunc = ErrorCode.errorToJson("email not exist")
        def enumUserAccunt(user_id : String)(successFunc : MongoDBObject => JsValue)(noneFunc : () => JsValue = noneFunc)(implicit errorFunc : JsValue = defaultErrorFunc) : JsValue = {
            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
              case Nil => noneFunc()
              case head :: Nil => {
                  (from db() in "accounts" where ("user_id" -> user_id) select (x => x)).toList match {
                    case Nil => successFunc(createAccount(0.asInstanceOf[Number], user_id))
                    case head :: Nil => successFunc(head)  
                    case _ => errorFunc
                  }
              }
              case _ => errorFunc
            }
        }
        
        def queryAccount(user_id : String, total : Float = 0) : Option[MongoDBObject] = {
          
            (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
              case Nil => None
              case head :: Nil => {
                  (from db() in "accounts" where ("user_id" -> user_id) select (x => x)).toList match {
                    case Nil => Some(createAccount(0.asInstanceOf[Number], user_id))
                    case head :: Nil => Some(head)  
                    case _ => None
                  }
              }
              case _ => None
            }
        }
        
        def pushMoneyImpl(owner_id: String, data : JsValue) : JsValue = {
            val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0)
            enumUserAccunt(owner_id)  { head => 
                    head += "balance" -> (head.getAs[Number]("balance").get.floatValue + total.asInstanceOf[Float]).asInstanceOf[Number]
                    _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                    DB2JsValue(head)
                } (noneFunc) 
        }
        
        def popMoneyImpl(owner_id : String, data : JsValue) : JsValue = {
            val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0)
            enumUserAccunt(owner_id)  { head => 
                val tmp = head.getAs[Number]("balance").get.floatValue - total.asInstanceOf[Float]
                if (tmp < 0) ErrorCode.errorToJson("not have enough money") 
                else {
                    head += "balance" -> tmp.asInstanceOf[Number]
                    _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                    DB2JsValue(head)
                }
            } (noneFunc) 
        }
    }
 
    import AccountModule.AccountModuleImpl._
    def queryAccount(user_id : String, data : JsValue) : JsValue = {
   
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        enumUserAccunt(user_id)  { head => 
            DB2JsValue(head)
        } { () => 
            DB2JsValue(createAccount(total.asInstanceOf[Number], user_id))
       }
    }
    
    def pushMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0)
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        if (AuthModule.adminAuthCheck(user_id)) {
            pushMoneyImpl(owner_id, data)
        } else ErrorCode.errorToJson("not have enough mana")
    }

    def popMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        if (AuthModule.adminAuthCheck(user_id)) {
            popMoneyImpl(owner_id, data)
        } else ErrorCode.errorToJson("not have enough mana")
    }
}