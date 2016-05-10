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
  		new ErrorNode("application not exist", -6, "申请不存在"),
  		new ErrorNode("not have enough mana", -7, "只有管理员能做这个操作"),
  		new ErrorNode("not have enough money", -8, "余额不足"),
  		new ErrorNode("not support coin", -9, "不支持当前币种"),
  		new ErrorNode("not enough coin", -10, "货存不足"),
  		new ErrorNode("not enough money", -11, "钱不够"),
  		new ErrorNode("user not approved", -12, "用户需要验证才有这个功能"),
  		new ErrorNode("user not admin", -13, "不是管理员账户"),
  		new ErrorNode("wrong input", -14, "输入不正确"),
  		new ErrorNode("currency not exist", -15, "货币不存在"),
  		new ErrorNode("currency is exist", -16, "货币已经存在"),
  		new ErrorNode("up to limit", -17, "取钱超出上限"),
  		new ErrorNode("not support money", -18, "暂时不支持人民币以外的钱"),

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