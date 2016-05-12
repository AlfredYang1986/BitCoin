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
import module.currency.CurrencyModule.currentCommition
import module.statistic.StatisticModule.pushIncome

object AccountModule {

    object AccountModuleImpl {
        def createAccount(total : Number, user_id : String) : MongoDBObject = {
            val builder = MongoDBObject.newBuilder
                  builder += "balance" -> total
                  builder += "freeze" -> total
                  builder += "total" -> total
                  builder += "btc" -> 0
                  builder += "ltc" -> 0
                  builder += "user_id" -> user_id
                 
                  _data_connection.getCollection("accounts") += builder.result
                  builder.result
        }
      
        def DB2JsValue(x : MongoDBObject) : JsValue =
            toJson(Map("balance" -> toJson(x.getAs[Number]("balance").get.floatValue),
                      "total" -> toJson(x.getAs[Number]("total").get.floatValue), 
                      "freeze" -> toJson(x.getAs[Number]("freeze").get.floatValue), 
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
                    head += "total" -> (head.getAs[Number]("total").get.floatValue + total.asInstanceOf[Float]).asInstanceOf[Number]
                    _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                    DB2JsValue(head)
                } (noneFunc) 
        }
        
        def freezeMoneyImpl(owner_id: String, data : JsValue) : JsValue = {
            val amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.0)
            val commition = currentCommition * amount.asInstanceOf[Float] 
            enumUserAccunt(owner_id)  { head => 
                  val tmp = head.getAs[Number]("balance").get.floatValue - amount.asInstanceOf[Float] - commition
                  if (tmp < 0) ErrorCode.errorToJson("not have enough money") 
                  else {
                      head += "balance" -> tmp.asInstanceOf[Number]
                      head += "freeze" -> (head.getAs[Number]("freeze").get.floatValue + amount.asInstanceOf[Float] + commition).asInstanceOf[Number]
                      _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                      DB2JsValue(head)
                  }
            } (noneFunc) 
        }
        
        def unfreezeMoneyImpl(owner_id: String, data : JsValue) : JsValue = {
            val amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.0)
            val commition = currentCommition * amount.asInstanceOf[Float] 
            enumUserAccunt(owner_id)  { head => 
                  val tmp = head.getAs[Number]("freeze").get.floatValue - amount.asInstanceOf[Float] - commition
                  if (tmp < 0) ErrorCode.errorToJson("not have enough money") 
                  else {
                      head += "freeze" -> tmp.asInstanceOf[Number]
                      head += "balance" -> (head.getAs[Number]("balance").get.floatValue + amount.asInstanceOf[Float] + commition).asInstanceOf[Number]
                      _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                      DB2JsValue(head)
                  }
            } (noneFunc) 
        }
        
        def popMoneyImpl(owner_id : String, data : JsValue) : JsValue = {
            val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0)
            enumUserAccunt(owner_id)  { head => 
                val tmp = head.getAs[Number]("freeze").get.floatValue - total.asInstanceOf[Float]
                if (tmp < 0) ErrorCode.errorToJson("not have enough money") 
                else {
                    head += "freeze" -> tmp.asInstanceOf[Number]
                    head += "total" -> (head.getAs[Number]("total").get.floatValue - total.asInstanceOf[Float]).asInstanceOf[Number]
                    _data_connection.getCollection("accounts").update(DBObject("user_id" -> owner_id), head)
                    pushIncome(total.asInstanceOf[Float] * currentCommition)
                    DB2JsValue(head)
                }
            } (noneFunc) 
        }
    }
 
    import AccountModule.AccountModuleImpl._
    def queryAccount(user_id : String, data : JsValue) : JsValue = {
   
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        enumUserAccunt(user_id)  { head => 
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
              DB2JsValue(head))))
        } { () => 
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
              DB2JsValue(createAccount(total.asInstanceOf[Number], user_id)))))
       }
    }
    
    def pushMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0)
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        if (AuthModule.adminAuthCheck(user_id)) {
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(pushMoneyImpl(owner_id, data))))
        } else ErrorCode.errorToJson("not have enough mana")
    }
    
    def freezeMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0)
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(freezeMoneyImpl(owner_id, data))))
    }
    
    def unfreezeMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0)
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(unfreezeMoneyImpl(owner_id, data))))
    }

    def popMoney(user_id : String, data : JsValue) : JsValue = {
        val total = (data \ "total").asOpt[Float].map (x => x).getOrElse(0) 
        val owner_id = (data \ "owner_id").asOpt[String].map (x => x).getOrElse("")
        if (AuthModule.adminAuthCheck(user_id)) {
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(popMoneyImpl(owner_id, data))))
        } else ErrorCode.errorToJson("not have enough mana")
    }
}