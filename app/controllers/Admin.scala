package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import module.auth.AuthModule.queryProfileWithToken
import module.statistic.StatisticModule.statistics
import module.currency.CurrencyModule
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.applies.AppliesModule
import module.stoke.StokeModule.queryAllStoke
import module.order.OrderModule.queryAllOrders
import module.bank.BankModule.queryBankAccount

object Admin extends Controller {
    def adminLogin(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
       
        Ok(views.html.admin_login())
    }
  
    def adminUpdateCurrency = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(CurrencyModule.updateCurrency)(true))
    def adminCreateCurrency = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(CurrencyModule.createCurrency)(true))
    def adminCurrencyManager(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val cur = (CurrencyModule.currency \ "result").asOpt[List[JsValue]].get
            val email = (profile \ "email").asOpt[String].get
            if (email == "admin") Ok(views.html.admin_currency_manager(token)(cur))
            else Redirect("/admin/login")
        }
    }
    
    def adminAppliesManager(t : String, p : String) = Action { request => 
      
        val page = p.toInt
      
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val email = (profile \ "email").asOpt[String].get
            val user_id = (profile \ "user_id").asOpt[String].get
            if (AuthModule.adminAuthCheck(user_id)) {
                val applies = (AppliesModule.queryAllApplications(user_id, 
                                toJson(Map("take" -> toJson(20), "skip" -> toJson(20 * page)))) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.admin_applies_manager(token)(applies)(page.toString))
            }else Redirect("/admin/login")
        }
    }
    
    def adminAuthAppliesManager(t : String, p : String) = Action { request => 
                
        val page = p.toInt
      
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val email = (profile \ "email").asOpt[String].get
            val user_id = (profile \ "user_id").asOpt[String].get
            if (AuthModule.adminAuthCheck(user_id)) {
                val applies = (AppliesModule.queryAuthApplications(user_id, 
                                toJson(Map("take" -> toJson(10), "skip" -> toJson(10 * page)))))
                Ok(views.html.admin_auth_apply(token)(applies)(page.toString))
            }else Redirect("/admin/login")
        }
    }
    
    def adminStatistic(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
           
            if (AuthModule.adminAuthCheck(user_id)) {
                val statistic = (statistics(toJson("")) \ "result")
                Ok(views.html.admin_statistic(token)(statistic))
            }else Redirect("/admin/login")
        }
    }
    
    def adminTradeConfig(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
           
            if (AuthModule.adminAuthCheck(user_id)) {
                val statistic = (statistics(toJson("")) \ "result")
                Ok(views.html.admin_trade_config(token)(statistic))
            }else Redirect("/admin/login")
        }
    }
    
    def adminStoke(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
           
            if (AuthModule.adminAuthCheck(user_id)) {
                val stoke = (queryAllStoke(user_id, toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.admin_stoke_config(token)(stoke))
            }else Redirect("/admin/login")
        }
    }
    
    def adminOrder(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
           
            if (AuthModule.adminAuthCheck(user_id)) {
                val orders = (queryAllOrders(user_id, toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.admin_order_manager(token)(orders))
            }else Redirect("/admin/login")
        }
    }
    
    def adminBankAccount(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok(views.html.not_auth("请先登陆在进行有效操作"))
        else {
            val profile = (queryProfileWithToken(token) \ "result")
            val user_id = (profile \ "user_id").asOpt[String].get
           
            if (AuthModule.adminAuthCheck(user_id)) {
                val account = (queryBankAccount(user_id, toJson("")) \ "result" \ "account").asOpt[String].get
                Ok(views.html.admin_bank_account(token)(account))
            }else Redirect("/admin/login")
        } 
    }
}