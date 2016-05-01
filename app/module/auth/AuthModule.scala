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

//import scala.concurrent.Await
//import scala.concurrent.Future
//import scala.concurrent.ExecutionContext.Implicits._
//import scala.concurrent.duration._
//
//import akka.actor.Actor
//import akka.actor.Props
//import akka.pattern.ask
//import akka.util.Timeout
//import akka.actor.ActorRef

object AuthModule {
    def register(data : JsValue) : JsValue = {
        val email = (data \ "email").asOpt[String].map (x => x).getOrElse("")
        val pwd = (data \ "pwd").asOpt[String].map (x => x).getOrElse("")

        if (email.equals("") || pwd.equals("")) ErrorCode.errorToJson("email or password not validata")
        else {
            (from db() in ("users") where ("email" -> email) select (x => x)).toList match {
              case Nil => { 
                  val builder = MongoDBObject.newBuilder
                  builder += "email" -> email
                  builder += "pwd" -> pwd
                  val user_id = Sercurity.md5Hash(email)
                  builder += "user_id" -> user_id
//                  val token = Sercurity.md5Hash(email + pwd + Sercurity.getTimeSpanWithDay)
                  val token = Sercurity.md5Hash(email + pwd)
                  builder += "token" -> token
                  
                  _data_connection.getCollection("users") += builder.result
                  
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
    
    def authCheck(token : String) : Option[String] = {
       
        def authCheckAcc(t : String) : String = {
            (from db() in ("users") where ("token" -> t) select (x => x)).toList match {
              case Nil => null
              case head :: Nil => head.getAs[String]("user_id").get
              case _ => null
            }
        }
      
        authCheckAcc(token.substring("Basic ".length())) match {
          case x : String => Some(x)
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