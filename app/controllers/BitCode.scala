package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws._

import module.auth.RegisterApprovedStatus._
import module.auth.AuthModule.queryProfileWithToken

object BitCode extends Controller {

    /**
     * 注册
     */
    def register = Action {
        Ok(views.html.register())
    }
    
    /**
     * 实名认证
     */
    def safe_auth(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val app = (profile \ "status").asOpt[Int].get
            Ok(views.html.safe_auth(token)(
                if (app == approved.s) approved
                else if (app == notApproved.s) notApproved
                else approving
            )(profile))
        }
    }
    
    /**
     * 修改密码
     */
    def changePwd(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit 
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val app = (profile \ "status").asOpt[Int].get
            Ok(views.html.safe_change_pwd(token)(profile))
        }
    }
}