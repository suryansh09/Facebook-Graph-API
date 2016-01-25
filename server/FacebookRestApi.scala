package main.scala

import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor._
import spray.can.server.Stats
import spray.util._
import spray.http._
import HttpMethods._
import MediaTypes._
import spray.can.Http.RegisterChunkHandler
import akka.event.Logging
import akka.io.IO
import spray.can.Http
import spray.routing.HttpService
import spray.routing.RequestContext
import spray.httpx.SprayJsonSupport
import akka.remote._
import akka.pattern.ask
import akka.util.Timeout.durationToTimeout
import scala.concurrent.ExecutionContext.Implicits.global
import java.net.InetAddress
import akka.pattern.pipe
import scala.concurrent.duration.DurationInt
import com.typesafe.config.ConfigFactory
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import spray.http.HttpEntity
import spray.http.HttpEntity.apply
import spray.can.Http
import spray.http.ContentTypes
import spray.http.HttpMethods.GET
import spray.http.HttpMethods.POST
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.json.pimpAny
import spray.json.pimpString
import scala.collection.mutable.ArrayBuffer
import akka.actor.ActorRef
import java.io.FileOutputStream
import java.util.Calendar



class FacebookRestApi(server:ActorRef) extends Actor with ActorLogging {
  implicit val timeout: Timeout = 20.second 
  import context.dispatcher

  def receive = {
    
    case _: Http.Connected => 
      sender ! Http.Register(self)


       
    
    case Start =>
    {
      val senderMachine = sender                    
      var future: Future[String] = (server ? Start()).mapTo[String]
     
     future.onSuccess
     {
            case result: String =>
            
            val body = HttpEntity(ContentTypes.`application/json`, result.toString)
            senderMachine ! HttpResponse(entity = body)
     }
    }
      
      
    
	
    
      
    case HttpRequest(GET, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/GetFriendsList" =>
    { 
        
        val senderMachine = sender
        var userID = path.split("/").last
        var contentString = entity.data.asString 

        println("Retrieving FriendList for:"+userID)
              
        var future: Future[String] = (server ? GetFriendList(userID+"#$#$#$"+contentString)).mapTo[String]
       
       future.onSuccess
       {
            case result: String =>
            
              val body = HttpEntity(ContentTypes.`application/json`, result.toString)
              senderMachine ! HttpResponse(entity = body)
       }
    }
    

    case HttpRequest(GET, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/GetPublicKey" =>
    { 
        val senderMachine = sender
        var userID = path.split("/").last
        var contentString = entity.data.asString 

        println("Retrieving Public Keys for:"+userID)
              
        var future: Future[String] = (server ? GetPublicKey(userID+"#$#$#$"+contentString)).mapTo[String]
       
       future.onSuccess
       {
            case result: String =>
            
              val body = HttpEntity(ContentTypes.`application/json`, result.toString)
              senderMachine ! HttpResponse(entity = body)
       }
    } 
    
    case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/PagePosting" =>
      {

          val senderMachine = sender

          var userID = path.split("/").last
          var contentString = entity.data.asString

            println("Posting on Page from:"+userID)

            var future: Future[String] = (server ? PagePosting(userID+"#$#$#$"+contentString)).mapTo[String]

            future.onSuccess
            {
          case result: String =>
          val body = HttpEntity(ContentTypes.`application/json`, result)
            senderMachine ! HttpResponse(entity = body)
            }
      }

    case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/ProfilePosting" =>
      {

          val senderMachine = sender

          var userID = path.split("/").last
          var contentString = entity.data.asString

            println("Posting on Profile from:"+userID)

            var future: Future[String] = (server ? ProfilePosting(userID+"#$#$#$"+contentString)).mapTo[String]

            future.onSuccess
            {
          case result: String =>
          val body = HttpEntity(ContentTypes.`application/json`, result)
            senderMachine ! HttpResponse(entity = body)
            }
      }

    case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/ProfilePictureUpload" =>
      {

          val senderMachine = sender

          var userID = path.split("/").last
          var contentString = entity.data.asString

            println("Uploading pictures on Profile from:"+userID)

            var future: Future[String] = (server ? ProfilePictureUpload(userID+"#$#$#$"+contentString)).mapTo[String]

            future.onSuccess
            {
          case result: String =>
          val body = HttpEntity(ContentTypes.`application/json`, result)
            senderMachine ! HttpResponse(entity = body)
            }
      }

    case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/AlbumPictureUpload" =>
      {

          val senderMachine = sender

          var userID = path.split("/").last
          var contentString = entity.data.asString

            println("Uploading pictures to Album from:"+userID)

            var future: Future[String] = (server ? AlbumPictureUpload(userID+"#$#$#$"+contentString)).mapTo[String]

            future.onSuccess
            {
          case result: String =>
          val body = HttpEntity(ContentTypes.`application/json`, result)
            senderMachine ! HttpResponse(entity = body)
            }
      }

    case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/CreatePage" =>
      {

          val senderMachine = sender

          var userID = path.split("/").last
          var contentString = entity.data.asString

            println("Creating Page by:"+userID)

            var future: Future[String] = (server ? CreatePage(userID+"#$#$#$"+contentString)).mapTo[String]

            future.onSuccess
            {
          case result: String =>
          val body = HttpEntity(ContentTypes.`application/json`, result)
            senderMachine ! HttpResponse(entity = body)
            }
      }

      case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/CreateAlbum" =>
      {

          val senderMachine = sender

          var userID = path.split("/").last
          var contentString = entity.data.asString

            println("Creating Album by:"+userID)

            var future: Future[String] = (server ? CreateAlbum(userID+"#$#$#$"+contentString)).mapTo[String]

            future.onSuccess
            {
          case result: String =>
          val body = HttpEntity(ContentTypes.`application/json`, result)
            senderMachine ! HttpResponse(entity = body)
            }
      }

    case HttpRequest(POST, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/Register" =>
     {
        
        val senderMachine = sender
        var userID = path.split("/").last
        var contentString = entity.data.asString      
        println("Registering user:"+userID)    
        var future: Future[String] = (server ? Register(userID+"#$#$#$"+contentString)).mapTo[String]
     
       future.onSuccess
       {
            case result: String =>
            
              val body = HttpEntity(ContentTypes.`application/json`, result.toString)
              senderMachine ! HttpResponse(entity = body)
       }
     }
 
         






      case HttpRequest(GET, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/GetPost" =>
      {
        val senderMachine = sender
        var userID = path.split("/").last
        var contentString = entity.data.asString 

        println("Retrieving post for user :" +userID)
      
            
        var future: Future[String] = (server ? GetPost(userID+"#$#$#$"+contentString)).mapTo[String]
     
       future.onSuccess
       {
            case result: String =>
            
              val body = HttpEntity(ContentTypes.`application/json`, result.toString)
              senderMachine ! HttpResponse(entity = body)
       }
      }

      case HttpRequest(GET, Uri.Path(path), _, entity: HttpEntity.NonEmpty, _) if path startsWith "/GetPicture" =>
      {
        val senderMachine = sender
        var userID = path.split("/").last
        var contentString = entity.data.asString 

        println("Retrieving picture for user :" +userID)
      
            
        var future: Future[String] = (server ? GetPicture(userID+"#$#$#$"+contentString)).mapTo[String]
     
       future.onSuccess
       {
            case result: String =>
            
              val body = HttpEntity(ContentTypes.`application/json`, result.toString)
              senderMachine ! HttpResponse(entity = body)
       }
      }
 
        


  }

}


