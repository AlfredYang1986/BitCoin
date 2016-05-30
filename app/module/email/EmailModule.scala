/*
 * Copyright (C) 2012 47 Degrees, LLC
 * http://47deg.com
 * hello@47deg.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package module.email
 
import akka.actor._
import play.api.libs.concurrent.Akka
import play.api.Play.current
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._
import akka.routing.SmallestMailboxRouter
import org.apache.commons.mail._
import akka.actor.ActorDSL._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import play.core.Router
import play.api.Play

/**
 * Smtp config
 * @param tls if tls should be used with the smtp connections
 * @param ssl if ssl should be used with the smtp connections
 * @param port the smtp port
 * @param host the smtp host name
 * @param user the smtp user
 * @param password thw smtp password
 */
case class SmtpConfig(tls : Boolean = false,
                      ssl : Boolean = false,
                      port : Int = 25,
                      host : String,
                      user : String,
                      password: String)
 
/**
 * The email message sent to Actors in charge of delivering email
 *
 * @param subject the email subject
 * @param recipient the recipient
 * @param from the sender
 * @param text alternative simple text
 * @param html html body
 */
case class EmailMessage(
                         subject: String,
                         recipient: String,
                         from: String,
                         text: String,
                         html: String,
                         smtpConfig : SmtpConfig,
                         retryOn: FiniteDuration,
                         var deliveryAttempts: Int = 0)
 
/**
 * Email service
 */
object EmailModule {
 
  /**
   * Uses the smallest inbox strategy to keep 20 instances alive ready to send out email
   * @see SmallestMailboxRouter
   */
  val emailServiceActor = Akka.system.actorOf(
    Props[EmailServiceActor].withRouter(
      SmallestMailboxRouter(nrOfInstances = 50)
    ), name = "emailService"
  )
 
  /**
   * public interface to send out emails that dispatch the message to the listening actors
   * @param emailMessage the email message
   */
  def sendEmail(data : JsValue) : JsValue = {
	  val user_email = (data \ "email").asOpt[String].get
	  val token = (data \ "token").asOpt[String].map (x => x).getOrElse("")
	 
    emailServiceActor ! (user_email, "")
	  Json.toJson(Map("status" -> toJson("ok"), "result" -> toJson("send email success")))
  }
  
  def send(emailMessage: EmailMessage) {
    emailServiceActor ! emailMessage
  }
  
  private def sendEmailToSync(to : String, content: String) {
      val email = new SimpleEmail
    	email.setHostName("smtp.qq.com")
    	email.setSmtpPort(25)
    	email.setAuthenticator(new DefaultAuthenticator("1919310800", ""))
    	email.setSSLOnConnect(true)
    	email.setFrom("1919310800@qq.com")
    	email.setSubject("邮件验证")
    	email.setMsg(content)
      email.addTo(to)
    	email.send
  }
 
  /**
   * An Email sender actor that sends out email messages
   * Retries delivery up to 10 times every 5 minutes as long as it receives
   * an EmailException, gives up at any other type of exception
   */
  class EmailServiceActor extends Actor with ActorLogging {
 
    /**
     * The actor supervisor strategy attempts to send email up to 10 times if there is a EmailException
     */
    override val supervisorStrategy =
      OneForOneStrategy(maxNrOfRetries = 10) {
        case emailException: EmailException => {
          log.debug("Restarting after receiving EmailException : {}", emailException.getMessage)
          Restart
        }
        case unknownException: Exception => {
          log.debug("Giving up. Can you recover from this? : {}", unknownException)
          Stop
        }
        case unknownCase: Any => {
          log.debug("Giving up on unexpected case : {}", unknownCase)
          Stop
        }
      }
 
    /**
     * Forwards messages to child workers
     */
    def receive = {
      case message: Any => context.actorOf(Props[EmailServiceWorker]) ! message
    }
 
  }
 
  /**
   * Email worker that delivers the message
   */
  class EmailServiceWorker extends Actor with ActorLogging {
 
    /**
     * The email message in scope
     */
    private var emailMessage: Option[EmailMessage] = None
 
    /**
     * Delivers a message
     */
    def receive = {
      case to : String => {
        log.debug("Atempting to deliver message")
        sendEmailToSync(to, "")
        log.debug("Message delivered")
      }
      case (to, content) => {
        log.debug("Atempting to deliver message")
        sendEmailToSync(to.asInstanceOf[String], content.asInstanceOf[String])
        log.debug("Message delivered")
      }
      case unexpectedMessage: Any => {
        log.debug("Received unexepected message : {}", unexpectedMessage)
        throw new Exception("can't handle %s".format(unexpectedMessage))
      }
    }
 
    /**
     * If this child has been restarted due to an exception attempt redelivery
     * based on the message configured delay
     */
    override def preRestart(reason: Throwable, message: Option[Any]) {
      if (emailMessage.isDefined) {
        log.debug("Scheduling email message to be sent after attempts: {}", emailMessage.get)
        import context.dispatcher
        // Use this Actors' Dispatcher as ExecutionContext
 
        context.system.scheduler.scheduleOnce(emailMessage.get.retryOn, self, emailMessage.get)
      }
    }
 
    override def postStop() {
      if (emailMessage.isDefined) {
        log.debug("Stopped child email worker after attempts {}, {}", emailMessage.get.deliveryAttempts, self)
      }
    }
 
  }
 
}