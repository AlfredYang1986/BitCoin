package module.auth

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
import module.statistic.StatisticModule.pushNewUsers

object IDType {
  case object socialID extends IDTypeDefines(0, "身份证")
  case object militaryID extends IDTypeDefines(1, "军官证")
  case object passportID extends IDTypeDefines(2, "护照")
  case object other extends IDTypeDefines(3, "其它")
}

sealed abstract class IDTypeDefines(val s : Int, val des : String)

object RegisterApprovedStatus {
  case object notApproved extends RegisterApprovedDefines(0, "未验证")
  case object approved extends RegisterApprovedDefines(1, "已验证")
  case object approving extends RegisterApprovedDefines(2, "审核中")
}

sealed abstract class RegisterApprovedDefines(val s : Int, val des : String)

object EmailStatus {
  case object notChecked extends EmailStatusDefines(0, "未验证")
  case object checked extends EmailStatusDefines(1, "已验证")
}

sealed abstract class EmailStatusDefines(val s : Int, val des : String)

object AuthModule {
    def register(data : JsValue) : JsValue = {
        val email = (data \ "email").asOpt[String].map (x => x).getOrElse("")
        val pwd = (data \ "pwd").asOpt[String].map (x => x).getOrElse("")

        val trade_pwd = (data \ "pwd_trade").asOpt[String].map (x => x).getOrElse("")
        val name = (data \ "name").asOpt[String].map (x => x).getOrElse("")
        val register_id = (data \ "register_id").asOpt[String].map (x => x).getOrElse(0)
        val id_type = (data \ "id_type").asOpt[Int].map (x => x).getOrElse(0)

        if (email.equals("") || pwd.equals("")) ErrorCode.errorToJson("email or password not validata")
        else {
            (from db() in ("users") where ("email" -> email) select (x => x)).toList match {
              case Nil => {
                  import IDType._
                  import EmailStatus._
                  import RegisterApprovedStatus._
                  val builder = MongoDBObject.newBuilder
                  builder += "email" -> email
                  builder += "email_status" -> notChecked.s
                  builder += "pwd" -> pwd
                  val user_id = Sercurity.md5Hash(email)
                  builder += "user_id" -> user_id
                  val token = Sercurity.md5Hash(email + pwd)
                  builder += "token" -> token
                 
                  builder += "name" -> name
                  builder += "trade_pwd" -> trade_pwd
                  builder += "id_type" -> id_type
                  builder += "register_id" -> register_id
                  builder += "approved_date" -> 0
                  builder += "status" -> notApproved.s
                  builder += "bank_accounts" -> MongoDBList.newBuilder.result
                  
                  _data_connection.getCollection("users") += builder.result
                  pushNewUsers
                  Json.toJson(Map("status" -> toJson("ok"), 
                      "result" -> toJson(Map("user_id" -> toJson(user_id), "token" -> toJson(token)))))
              }
              case _ => ErrorCode.errorToJson("email already reg")
            }
        }
    }
    
    def login(data : JsValue) : JsValue = {
        val email = (data \ "email").asOpt[String].map(x => x).getOrElse("")
        val token = (data \ "token").asOpt[String].map(x => x).getOrElse("")
       
        (from db() in ("users") where ("email" -> email) select (x => x)).toList match {
          case Nil => ErrorCode.errorToJson("email not exist")
          case head :: Nil => {
               val validata = Sercurity.md5Hash(email + head.getAs[String]("pwd").get)
               if (token.equals(validata)) 
                   Json.toJson(Map("status" -> toJson("ok"), 
                                   "result" -> toJson(Map("user_id" -> toJson(head.getAs[String]("user_id").get), 
                                   "token" -> toJson(token)))))
               else ErrorCode.errorToJson("email not exist") 
          }
          case _ => ErrorCode.errorToJson("unknown error")
        }
    }
    
    def admainLogin(data : JsValue) : JsValue = {
        val email = (data \ "email").asOpt[String].map(x => x).getOrElse("")
        val token = (data \ "token").asOpt[String].map(x => x).getOrElse("")
       
        if (email != "admin") ErrorCode.errorToJson("user not admin")
        else {
            (from db() in "users" where ("email" -> email) select (x => x)).toList match {
              case Nil => ErrorCode.errorToJson("email not exist")
              case head :: Nil => {
                   val validata = Sercurity.md5Hash(email + head.getAs[String]("pwd").get)
                   if (token.equals(validata)) 
                       Json.toJson(Map("status" -> toJson("ok"), 
                                       "result" -> toJson(Map("user_id" -> toJson(head.getAs[String]("user_id").get), 
                                       "token" -> toJson(token)))))
                   else ErrorCode.errorToJson("email not exist") 
              }
              case _ => ErrorCode.errorToJson("unknown error")
            }
        }
    }
  
    def queryBankAccount(user_id : String, Data : JsValue) : JsValue = 
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
          (from db() in "users" where ("user_id" -> user_id) select (x => x.getAs[MongoDBList]("bank_accounts").get)).toList match {
            case Nil => ErrorCode.errorToJson("email not exist")
            case head :: Nil => toJson(head.toList.asInstanceOf[List[MongoDBObject]] map (account2JsValue(_)))
            case _ => ErrorCode.errorToJson("email not exist")
          })))
    
    def account2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("bank_name" -> toJson(x.getAs[String]("bank_name").get),
                   "bank_account" -> toJson(x.getAs[String]("bank_account").get),
                   "account_name" -> toJson(x.getAs[String]("account_name").get)))
    
    def DB2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                   "token" -> toJson(x.getAs[String]("token").get),
                   "email" -> toJson(x.getAs[String]("email").get),
                   "email_status" -> toJson(x.getAs[Number]("email_status").get.intValue),
                   "name" -> toJson(x.getAs[String]("name").get),
                   "type" -> toJson(x.getAs[Number]("id_type").get.intValue),
                   "register_id"-> toJson(x.getAs[String]("register_id").get),
                   "status"-> toJson(x.getAs[Number]("status").get.intValue),
                   "approved_date"-> toJson(x.getAs[Number]("approved_date").get.longValue)))
    
    def queryProfile(user_id : String, data : JsValue) : JsValue = {
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "users" where ("user_id" -> user_id) select (DB2JsValue(_))).toList.head)))
    }
    
    def queryProfileWithToken(token : String) : JsValue = {
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "users" where ("token" -> token) select (DB2JsValue(_))).toList.head)))
    }
    
    def updateProfile(user_id : String, data : JsValue) : JsValue = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
          case head :: Nil => {
              (data \ "pwd").asOpt[String].map { x => 
                 head += "pwd" -> x 
                 val email = head.getAs[String]("email").get
                 head += "token" -> Sercurity.md5Hash(email + x) 
              }.getOrElse(Unit)
              (data \ "name").asOpt[String].map (x => head += "name" -> x).getOrElse(Unit)
              (data \ "register_id").asOpt[String].map (x => head += "register_id" -> x).getOrElse(Unit)
              (data \ "id_type").asOpt[Int].map (x => head += "id_type" -> x.asInstanceOf[Number]).getOrElse(Unit)
              (data \ "status").asOpt[Int].map (x => head += "status" -> x.asInstanceOf[Number]).getOrElse(Unit)
              (data \ "approved_date").asOpt[Long].map (x => head += "approved_date" -> x.asInstanceOf[Number]).getOrElse(Unit)
              (data \ "trade_pwd").asOpt[String].map (x => head += "trade_pwd" -> x).getOrElse(Unit)
              
              _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), head)

              toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(head))))
          }
          case Nil => ErrorCode.errorToJson("email not exist") 
          case _ => ErrorCode.errorToJson("email not exist") 
        }
    }
    
    def queryMultipleProfiles(lst : List[String]) : List[JsValue] = {
        def conditionImpl(l : List[String], cur: Option[DBObject]) : Option[DBObject] = {
            l match {
              case Nil => cur
              case head :: t => cur match {
                                      case None => conditionImpl(t, Some(DBObject("user_id" -> head)))
                                      case Some(x) => conditionImpl(t, Some($or(x, DBObject("user_id" -> head))))
                                  }
            }
        }
       
        conditionImpl(lst, None) match {
          case None => Nil
          case Some(x) => (from db() in "users" where (x) select (DB2JsValue(_))).toList
        }
    }
   
    def adminAuthCheck(user_id : String) : Boolean = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x.getAs[String]("email").get)).toList match {
          case Nil => false
          case head :: Nil => head.equals("admin")
          case _ => false
        }
    }
    
    def authCheck(token : String) : Option[(String, Int)] = {
       
        def authCheckAcc(t : String) : (String, Int) = {
            (from db() in ("users") where ("token" -> t) select (x => x)).toList match {
              case Nil => null
              case head :: Nil => (head.getAs[String]("user_id").get, head.getAs[Number]("status").map (x => x.intValue).getOrElse(RegisterApprovedStatus.notApproved.s))
              case _ => null
            }
        }
      
        authCheckAcc(token.substring("Basic ".length())) match {
          case x : (String, Int) => Some(x)
          case null => None
        }
    }
    
    def authCheckUser(user_id : String)(x : JsValue)(func : (String, JsValue) => JsValue) : JsValue = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
          case Nil => ErrorCode.errorToJson("email not exist")
          case head :: Nil => func(user_id, x)
          case _ => ErrorCode.errorToJson("email already reg")
        }
    }
}