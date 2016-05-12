
import play.api.GlobalSettings
import play.api.Application

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity

import akka.actor.{Actor, Props}
import play.api.libs.concurrent.Akka
import play.api.GlobalSettings
import play.api.templates.Html
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import module.timer.handleOrders
import module.timer.TimerModule

object Global extends GlobalSettings {
    override def onStart(application : Application) = {
        if (!_data_connection.isExisted("users")) {
           import module.auth.RegisterApprovedStatus._
           import module.auth.IDType._
          
           val email = "admin"
           val pwd = "admin"
          
           val builder = MongoDBObject.newBuilder
           builder += "email" -> email
           builder += "pwd" -> pwd
           val user_id = Sercurity.md5Hash(email)
           builder += "user_id" -> user_id
           val token = Sercurity.md5Hash(email + pwd)
           builder += "token" -> token
           
           builder += "name" -> "admin"
           builder += "id_type" -> socialID.s
           builder += "register_id" -> ""
           builder += "approved_date" -> 0
           builder += "status" -> approved.s
                  
           _data_connection.getCollection("users") += builder.result 
        }
        
    		import scala.concurrent.duration._
    		val actor = Akka.system(application).actorOf(Props[TimerModule])
		    Akka.system(application).scheduler.schedule(0.seconds, 10.seconds, actor, handleOrders) 
    }
}