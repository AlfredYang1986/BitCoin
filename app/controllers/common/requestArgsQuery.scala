package controllers.common

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.Files.TemporaryFile

import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.auth.AuthModule.authCheckUser

object requestArgsQuery extends Controller {
  def requestArgs(request : Request[AnyContent])(func : JsValue => JsValue) : Result = {
  		try {
  			request.body.asJson.map { x => 
  			    Ok(func(x))
  			}.getOrElse (BadRequest("Bad Request for input"))
  		} catch {
  			case _ : Exception => BadRequest("Bad Request for input")
  		}  		   
	}
  
//  def requestArgsAcc(request : Request[AnyContent])(func : JsValue => JsValue) : JsValue = {
//  		try {
//  			request.body.asJson.map { x => 
//  				func(x)
//  			}.getOrElse (toJson("Bad Request for input"))
//  		} catch {
//  			case _ : Exception => (toJson("Bad Request for input"))
//  		}  		   
//	} 
  
  	def requestGetRequestArgs(request :Request[AnyContent])(auth : String => Option[(String, Int)])(func : (String, JsValue) => JsValue)(bNeedApproved : Boolean) : Result = {
       import module.auth.RegisterApprovedStatus._
  	   try {
  	     request.body.asJson.map { x => 
  	         request.headers.get("Authorization").map (auth(_)).getOrElse(None) match {
  	           case Some((user_id, status)) => if ((bNeedApproved && status == approved.s) || !bNeedApproved)  Ok(authCheckUser(user_id)(x)(func))
  	                                           else Ok(ErrorCode.errorToJson("user not approved"))
  	           case None => Ok(ErrorCode.errorToJson("email not exist"))
  	         }
  	     }.getOrElse(BadRequest)
  	   } catch {
  	     case _ : Exception => BadRequest
  	   }
  	}
}