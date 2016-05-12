package module.order

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.concurrent._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.account.AccountModule.{ freezeMoney, queryAccount, unfreezeMoney }
import java.util.Date
import module.stoke.StokeModule.{ purchaseStoke, sellMyStoke }
import module.auth.AuthModule.adminAuthCheck

import module.stoke.CoinType._

object OrderTypes {
  case object purchase extends OrderTypesDefines(0, "order purchase")
  case object sell extends OrderTypesDefines(1, "order sells")
}

sealed abstract class OrderTypesDefines(val t : Int, val des : String)

object OrderStatus {
  case object waiting extends OrderTypesDefines(0, "order waiting")
  case object success extends OrderTypesDefines(1, "order success")
  case object reject extends OrderTypesDefines(2, "order reject")
}

sealed abstract class OrderStatusDefines(val t : Int, val des : String)

object OrderModule {
    import OrderTypes._
    import OrderStatus._
    def pushOrder(user_id : String, data : JsValue) : JsValue = {
        val order_type = (data \ "type").asOpt[Int].map (x => x).getOrElse(purchase.t)
        val coin_type = (data \ "coin").asOpt[Int].map (x => x).getOrElse(BTC.s)
        val amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.floatValue)
        val price = (data \ "price").asOpt[Float].map (x => x).getOrElse(0.floatValue)
        val order_id = Sercurity.md5Hash(user_id + Sercurity.getTimeSpanWithMillSeconds)
        
        val builder = MongoDBObject.newBuilder
        builder += "amount" -> amount
        builder += "price" -> price 
        builder += "coin_type" -> BTC.s
        builder += "type" -> order_type
        builder += "status" -> waiting.t
        builder += "order_user_id" -> user_id
        builder += "date" -> new Date().getTime
        builder += "order_id" -> order_id
        
        val result = builder.result
        
        val reVal : JsValue = 
            if (order_type == purchase.t) {
                if ((queryAccount(user_id, toJson("")) \ "result" \ "balance").asOpt[Float].get < amount * price ) ErrorCode.errorToJson("not enough money")
                else {
                    freezeMoney(user_id, 
                        toJson(Map("amount" -> toJson(result.getAs[Number]("amount").get.floatValue * result.getAs[Number]("price").get.floatValue),
                                   "owner_id" -> toJson(result.getAs[String]("order_user_id").get))))
                }
            } else if (order_type == sell.t) {
               val owns = if (coin_type == BTC.s) (queryAccount(user_id, toJson("")) \ "result" \ "btc").asOpt[Float].get 
                          else if (coin_type == LTC.s) (queryAccount(user_id, toJson("")) \ "result" \ "ltc").asOpt[Float].get 
                          else 0.toFloat
                          
               if (owns < amount) ErrorCode.errorToJson("not enough coin")
               else null
            } else null
        
        if (reVal != null && ((reVal \ "status").asOpt[String].get != "ok")) reVal
        else {
            _data_connection.getCollection("order") += result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("order_id" -> toJson(order_id)))))
        }
    }
   
    def DB2JsValue(obj : MongoDBObject) : JsValue = 
        toJson(Map("order_id" -> toJson(obj.getAs[String]("order_id").get),
                   "coin_type" -> toJson(obj.getAs[Number]("coin_type").get.intValue),
                   "amount" -> toJson(obj.getAs[Number]("amount").get.floatValue),
                   "price" -> toJson(obj.getAs[Number]("price").get.floatValue),
                   "order_type" -> toJson(obj.getAs[Number]("type").get.intValue),
                   "status" -> toJson(obj.getAs[Number]("status").get.intValue),
                   "order_user_id" -> toJson(obj.getAs[String]("order_user_id").get),
                   "date" -> toJson(obj.getAs[Number]("date").get.longValue)))
    
    def queryOrders(user_id : String, data : JsValue) : JsValue = {
        val buys = (from db() in "order" where ("type" -> purchase.t, "status" -> waiting.t)).selectTop(10)("price")(DB2JsValue(_)).toList
        val sells = (from db() in "order" where ("type" -> sell.t, "status" -> waiting.t)).selectTop(10)("price")(DB2JsValue(_)).toList
             
        toJson(Map("status" -> toJson("ok"), "result" -> 
            toJson(Map("buys" -> toJson(buys), 
                       "sells" -> toJson(sells)))))
    }
    
    def queryPurchaseBTC(p : Float) : List[String] = 
        (from db() in "order" where ("type" -> purchase.t, "status" -> waiting.t, "coin_type" -> BTC.s)).
            select (x =>(x.getAs[String]("order_id").get, x.getAs[Number]("price").get.floatValue)).toList.
               filter(x => Math.round(10.0 * (x._2 - p)) == 0).map(x => x._1)
               
    def queryPurchaseLTC(p : Float) : List[String] = 
        (from db() in "order" where ("type" -> purchase.t, "status" -> waiting.t, "coin_type" -> LTC.s)).
            select (x =>(x.getAs[String]("order_id").get, x.getAs[Number]("price").get.floatValue)).toList.
               filter(x => Math.round(10.0 * (x._2 - p)) == 0).map(x => x._1)

    def querySellBTC(p : Float) : List[String] = 
        (from db() in "order" where ("type" -> sell.t, "status" -> waiting.t, "coin_type" -> BTC.s)).
            select (x =>(x.getAs[String]("order_id").get, x.getAs[Number]("price").get.floatValue)).toList.
               filter(x => Math.round(10.0 * (x._2 - p)) == 0).map(x => x._1)

    def querySellLTC(p : Float) : List[String] = 
        (from db() in "order" where ("type" -> sell.t, "status" -> waiting.t, "coin_type" -> LTC.s)).
            select (x =>(x.getAs[String]("order_id").get, x.getAs[Number]("price").get.floatValue)).toList.
               filter(x => Math.round(10.0 * (x._2 - p)) == 0).map(x => x._1)
    
    def queryAllOrders(user_id : String, data : JsValue) : JsValue = {
        val reVal = (from db() in "order" select (DB2JsValue(_))).toList.reverse
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(reVal)))
    }
    
    def queryMyOrders(user_id : String, data : JsValue) : JsValue = {
        val reVal = (from db() in "order" where ("order_user_id" -> user_id)).select(DB2JsValue(_)).toList
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(reVal)))
    }
    
    def rejectOrders(user_id : String, data : JsValue) : JsValue = {
        val order_id = (data \ "order_id").asOpt[String].map (x => x).getOrElse("")
       
        if (order_id.isEmpty) ErrorCode.errorToJson("application not exist")
        else {
            (from db() in "order" where ("order_id" -> order_id) select (x => x)).toList match {
              case Nil => ErrorCode.errorToJson("application not exist")
              case head :: Nil => {
                  head += "status" -> OrderStatus.reject.t.asInstanceOf[Number]
                  _data_connection.getCollection("order").update(DBObject("order_id" -> order_id), head)
                    
                  if (head.getAs[Int]("type").get.intValue == purchase.t) {
                      unfreezeMoney(user_id, 
                            toJson(Map("amount" -> toJson(head.getAs[Number]("amount").get.floatValue * head.getAs[Number]("price").get.floatValue),
                                       "owner_id" -> toJson(head.getAs[String]("order_user_id").get))))
                  } else Unit
                  
                  toJson(Map("status" -> "ok", "result" -> "revert application success"))
              }
              case _ => ErrorCode.errorToJson("application not exist")
            }
        }
    }
    
    def approveOrders(user_id : String, data : JsValue) : JsValue = {
        val order_id = (data \ "order_id").asOpt[String].get
        confirmOrder(order_id)
    }
    
    def confirmOrder(order_id : String) : JsValue = {
        if (order_id.isEmpty) ErrorCode.errorToJson("application not exist")
        else {
            (from db() in "order" where ("order_id" -> order_id) select (x => x)).toList match {
              case Nil => ErrorCode.errorToJson("application not exist")
              case head :: Nil => {
                println(head)
                  val user_id = head.getAs[String]("order_user_id").get

                  val result = 
                      if (head.getAs[Int]("type").get.intValue == purchase.t) {
                            purchaseStoke(user_id, 
                                toJson(Map("amount" -> toJson(head.getAs[Number]("amount").get.floatValue),
                                           "type" -> toJson(head.getAs[Int]("coin_type").get.intValue),
                                           "price" -> toJson(head.getAs[Number]("price").get.floatValue))))
                      } else {
                            sellMyStoke(user_id, 
                                toJson(Map("amount" -> toJson(head.getAs[Number]("amount").get.floatValue),
                                           "type" -> toJson(head.getAs[Int]("coin_type").get.intValue),
                                           "price" -> toJson(head.getAs[Number]("price").get.floatValue))))
                      }
                  
                  if ((result \ "status").asOpt[String].get == "ok") {
                      head += "status" -> OrderStatus.success.t.asInstanceOf[Number]
                      _data_connection.getCollection("order").update(DBObject("order_id" -> order_id), head)
                      toJson(Map("status" -> "ok", "result" -> "confirm order success"))
                  } else result
              }
              case _ => ErrorCode.errorToJson("application not exist")
            }
        }
    }
}