package util.errorcode

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue

object ErrorCode {
  	case class ErrorNode(name : String, code : Int, message : String)

  	private def xls : List[ErrorNode] = List(
  		new ErrorNode("email or password not validata", -1, "请输入正确的邮件和密码"),
  		new ErrorNode("email already reg", -2, "该邮箱已经被注册"),
  		new ErrorNode("email not exist", -3, "用户不存在"),
  		new ErrorNode("pwd error", -4, "密码错误"),
  		new ErrorNode("report not exist", -5, "官方公告不存在"),

  		new ErrorNode("unknown error", -999, "unknown error")
  	)
  
  	def getErrorCodeByName(name : String) : Int = (xls.find(x => x.name == name)) match {
  			case Some(y) => y.code
  			case None => -9999
  		}
  	
   	def getErrorMessageByName(name : String) : String = (xls.find(x => x.name == name)) match {
  			case Some(y) => y.message
  			case None => "unknow error"
  		}
   	
   	def errorToJson(name : String) : JsValue =
  		Json.toJson(Map("status" -> toJson("error"), "error" -> 
  				toJson(Map("code" -> toJson(this.getErrorCodeByName(name)), "message" -> toJson(this.getErrorMessageByName(name))))))
}