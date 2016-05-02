package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.applies.AppliesModule

object ApplyController extends Controller {
    def pushApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.pushApplications)(false))
    def revertApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.revertApplications)(false))
    def approveApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.approveApplications)(true))
    def rejectApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.rejectApplications)(true))
    
    def queryAllApps = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.queryAllApplications)(true))
    def queryMyApps = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.queryMyApplications)(true))
}