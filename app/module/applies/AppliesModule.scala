package module.applies

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.concurrent._

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import java.util.Date

import module.auth.AuthModule

object ApplyTypes {
  case object pushMoney extends ApplyTypesDefines(0, "push money")
  case object popMoney extends ApplyTypesDefines(1, "pop money")
  case object accountApp extends ApplyTypesDefines(2, "upgrade account")
}

sealed abstract class ApplyTypesDefines(val t : Int, val des : String)

object ApplyStatus {
  case object add extends ApplyStatusDefines(0, "add")
  case object approve extends ApplyStatusDefines(1, "approve")
  case object revert extends ApplyStatusDefines(2, "revert")
  case object reject extends ApplyStatusDefines(3, "reject")
}

sealed abstract class ApplyStatusDefines(val s : Int, val des : String)

object AppliesModule {
    def pushApplications(user_id : String, data : JsValue) : JsValue = {
        val app_type = (data \ "type").asOpt[Int].map (x => x).getOrElse(ApplyTypes.pushMoney.t)
        val amount = (data \ "amount").asOpt[Float].map (x => x).getOrElse(0.floatValue)
        val message = (data \ "message").asOpt[String].map (x => x).getOrElse("")
        val apply_id = Sercurity.md5Hash(user_id + Sercurity.getTimeSpanWithMillSeconds)
        
        val builder = MongoDBObject.newBuilder
        builder += "message" -> message
        builder += "amount" -> amount
        builder += "type" -> app_type
        builder += "status" -> ApplyStatus.add.s
        builder += "apply_user_id" -> user_id
        builder += "date" -> new Date().getTime
        builder += "apply_id" -> apply_id
        
        import module.applies.ApplyTypes._
        if (app_type == accountApp.t) {
            module.auth.AuthModule.updateProfile(user_id, toJson(Map("status" -> 2)))
        }
       
        import module.account.AccountModule.queryAccount
        if (app_type == popMoney.t && (queryAccount(user_id, toJson("")) \ "balance").asOpt[Float].get < amount ) {
            ErrorCode.errorToJson("not enough money")
        } else {
            _data_connection.getCollection("apply") += builder.result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(Map("apply_id" -> toJson(apply_id)))))
        }
    }
    
    def revertApplications(user_id : String, data : JsValue) : JsValue = {
        val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse("")
       
        if (apply_id.isEmpty) ErrorCode.errorToJson("application not exist")
        else {
            (from db() in "apply" where ("apply_id" -> apply_id) select (x => x)).toList match {
              case Nil => ErrorCode.errorToJson("application not exist")
              case head :: Nil => {
                  head += "status" -> ApplyStatus.revert.s.asInstanceOf[Number]
                  _data_connection.getCollection("apply").update(DBObject("apply_id" -> apply_id), head)
                
                  toJson(Map("status" -> "ok", "result" -> "revert application success"))
              }
              case _ => ErrorCode.errorToJson("application not exist")
            }
        }
    }
    
    def approveApplications(user_id : String, data : JsValue) : JsValue =
        if (AuthModule.adminAuthCheck(user_id)) {
            val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse("")
            
            if (apply_id.isEmpty) ErrorCode.errorToJson("application not exist")
            else {
                (from db() in "apply" where ("apply_id" -> apply_id) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("application not exist")
                  case head :: Nil => {
                      head += "status" -> ApplyStatus.approve.s.asInstanceOf[Number]
                      _data_connection.getCollection("apply").update(DBObject("apply_id" -> apply_id), head)
                      toJson(Map("status" -> "ok", "result" -> "approve application success"))
                  }
                  case _ => ErrorCode.errorToJson("application not exist")
                }
            }
        } else ErrorCode.errorToJson("not have enough mana")
        
    def rejectApplications(user_id : String, data : JsValue) : JsValue =
        if (AuthModule.adminAuthCheck(user_id)) {
            val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse("")
            
            if (apply_id.isEmpty) ErrorCode.errorToJson("application not exist")
            else {
                (from db() in "apply" where ("apply_id" -> apply_id) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("application not exist")
                  case head :: Nil => {
                      head += "status" -> ApplyStatus.reject.s.asInstanceOf[Number]
                  _data_connection.getCollection("apply").update(DBObject("apply_id" -> apply_id), head)
                      toJson(Map("status" -> "ok", "result" -> "reject application success"))
                  }
                  case _ => ErrorCode.errorToJson("application not exist")
                }
            }
        } else ErrorCode.errorToJson("not have enough mana")
    
    def DB2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("apply_id" -> toJson(x.getAs[String]("apply_id").get),
                   "apply_type" -> toJson(x.getAs[Number]("type").get.intValue),
                   "date" -> toJson(x.getAs[Number]("date").get.floatValue),
                   "status" -> toJson(x.getAs[Number]("status").get.intValue),
                   "amount" -> toJson(x.getAs[Number]("amount").get.floatValue),
                   "message" -> toJson(x.getAs[String]("message").get),
                   "apply_user_id" -> toJson(x.getAs[String]("apply_user_id").get)))
    
    def queryAllApplications(user_id : String, data : JsValue) : JsValue = 
        if (AuthModule.adminAuthCheck(user_id)) {
            val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
            val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
            
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                ((from db() in "apply" where ($or("type" -> ApplyTypes.pushMoney.t.asInstanceOf[Number], 
                                                  "type" -> ApplyTypes.popMoney.t.asInstanceOf[Number])
                                              )).selectSkipTop(skip)(take)("date")(x => 
                    DB2JsValue(x)
                )).toList))) 
        } else ErrorCode.errorToJson("not have enough mana")
        
    def queryAuthApplications(user_id : String, data : JsValue) : JsValue = 
        if (AuthModule.adminAuthCheck(user_id)) {
            val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
            val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
            
            val lst = ((from db() in "apply" where ("type" -> ApplyTypes.accountApp.t.asInstanceOf[Number]))
                        .selectSkipTop(skip)(take)("date")(x => x.getAs[String]("apply_user_id").get)).toList
      
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(AuthModule.queryMultipleProfiles(lst))))
        } else ErrorCode.errorToJson("not have enough mana")
    
    def queryMyApplications(user_id :String, data : JsValue) : JsValue = 
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "apply" where ("apply_user_id" -> user_id) select (x =>
                DB2JsValue(x)
            )).toList.reverse)))
}