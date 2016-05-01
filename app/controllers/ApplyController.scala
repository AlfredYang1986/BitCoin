package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.applies.AppliesModule

object ApplyController extends Controller {
    def pushApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.pushApplications))
    def revertApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.revertApplications))
    def approveApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.approveApplications))
    def rejectApp = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.rejectApplications))
    
    def queryAllApps = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.queryAllApplications))
    def queryMyApps = Action (request => requestGetRequestArgs(request)(AuthModule.authCheck)(AppliesModule.queryMyApplications))
}