package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import module.auth.RegisterApprovedStatus._
import module.auth.AuthModule.queryProfileWithToken
import module.account.AccountModule.queryAccount
import module.common.http.HTTP
import module.applies.AppliesModule.queryMyApplications

object BitCode extends Controller {

    /**
     * 注册
     */
    def register = Action {
        Ok(views.html.register())
    }
    
    /******************************************************/
    /**
     * 安全中心
     * 安全主页
     */
    def profileIndex(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            Ok(views.html.safe_index(token)(profile))
        }
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
    /******************************************************/
   
    /******************************************************/
    /**
     * 财务中心
     * 财务主页
     */
    def financeIndex(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit 
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
            val account = (queryAccount(user_id, toJson("")) \ "result") 
            val btc = (((HTTP("http://api.huobi.com/staticmarket/ticker_btc_json.js").
                       header("Accept" -> "application/json", "Content-Type" -> "application/json").
                       get(Map.empty)) \ "ticker") \ "last").asOpt[Float].get
            val ltc = (((HTTP("http://api.huobi.com/staticmarket/ticker_ltc_json.js").
                       header("Accept" -> "application/json", "Content-Type" -> "application/json").
                       get(Map.empty)) \ "ticker") \ "last").asOpt[Float].get
            Ok(views.html.finance_index(token)(profile)(account)(btc)(ltc))
        }
    }
    
    def financePay(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit 
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            Ok(views.html.finance_people_pay(token)(profile))
        }
    }
    
    def financeWithdraw(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit 
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            Ok(views.html.finance_people_withdraw(token)(profile))
        }
    }
    
    def financeQueryApp(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit 
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
            val applies = (queryMyApplications(user_id, toJson("")) \ "result").asOpt[List[JsValue]].get
            println(applies)
            Ok(views.html.finance_query_app(token)(profile)(applies))
        }
    }
    
    /******************************************************/
    
    /**
     * 主页面
     */
    def index(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit 
  
        import module.report.ReportModule.queryReports
        import module.bitnews.BitNews.queryBitNews
        import module.othernews.OtherNews.queryOtherNews
        val reports = (queryReports(toJson("")) \ "result").asOpt[List[JsValue]].get
        val bitnews = (queryBitNews(toJson("")) \ "result").asOpt[List[JsValue]].get
        val othernews = (queryOtherNews(toJson("")) \ "result").asOpt[List[JsValue]].get
        
        if (token == "") Ok(views.html.index("")(null)(reports)(bitnews)(othernews))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val app = (profile \ "status").asOpt[Int].get
            Ok(views.html.index(token)(profile)(reports)(bitnews)(othernews))
        }
    }
}