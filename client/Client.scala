package main.scala

import akka.actor._
import akka.actor.Terminated
import akka.actor.actorRef2Scala
import akka.actor.Actor
import akka.dispatch.Foreach
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.LoggingReceive 
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io._
import java.security.MessageDigest
import com.typesafe.config.ConfigFactory
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import akka.actor._
import spray.http.{ HttpRequest, HttpResponse }
import spray.client.pipelining.{ Get, sendReceive }
import spray.client.pipelining.{ Post, sendReceive }
import spray.http._
import spray.json.DefaultJsonProtocol
import spray.httpx.encoding.{Gzip, Deflate}
import spray.httpx.SprayJsonSupport._
import spray.client.pipelining._
import scala.concurrent.Future
import scala.util.{ Success, Failure }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import akka.actor.Props
import scala.util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set


  
trait WebClientGet {
    def get(url: String,entity:HttpEntity): Future[String]
}
  
  
class SprayWebClientGet(implicit system: ActorSystem) extends WebClientGet {
   
    
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
 
    
    def get(url: String,entity:HttpEntity): Future[String] = {
      val futureResponse = pipeline(Get(url,entity))
      futureResponse.map(_.entity.asString)
    }
}

class SprayWebClientPost(implicit system: ActorSystem) {
    val pipeline: HttpRequest => Future[String] = (
        sendReceive
        ~> unmarshal[String]
        )
}

class ClientActor(system:ActorSystem) extends Actor
{ 
	var serverPublicKey = ""
	var serverAuthenticationToken ="" 
	var tokenAuthentication = ""
	var keyPair = BaseEncryption.keyPairGeneration
	var publicKeyString = BaseEncryption.publicKeyToString(keyPair.getPublic)
	var username = self.path.name
    
    def receive = {
      case Start => {
        var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, publicKeyString)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/Register/"+username,entity))
        isPostSuccess onComplete {
          case Success(response) => {
          	println("Authentication Token Received from the Server for user:"+username)
          	var responseArray = response.split("#\\$#\\$#\\$")
          	serverPublicKey = responseArray(0)
          	serverAuthenticationToken = BaseEncryption.decryptionPrivate(responseArray(1),keyPair.getPrivate)
          	tokenAuthentication = BaseEncryption.encryptionPublic(serverAuthenticationToken,serverPublicKey)
          }
         
        }
      }
          
      case GetFriendsList => {
      	
      	var webClient = new SprayWebClientGet()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication)
        var hashResponse = webClient.get("http://127.0.0.1:8000/GetFriendsList/"+username,entity)
        
        hashResponse onComplete {
          case Success(response) => {
          	println(username+" Friend List: " +response) 
          }
        }
      }

      
      case PagePosting(msg:String, pageID:String, usrList:String) =>{
      	var userList = usrList+":"+username
      	var usr = userList.split(":")
      	var userNames = ""
      	var symmetricKey = BaseEncryption.generateSymmetricKey()
      	var encryptedMsg = BaseEncryption.encryptionSymmetric(msg,symmetricKey)
      	var encrptedSymmetricKey = ""
      	for(i <- usr){
      		if(!userNames.equals("")){
      			userNames += "####"
      			encrptedSymmetricKey += "####"
      		}
      		var usrPublicKey = Global.publicKeys.getOrElse(i,null)
      		
		    encrptedSymmetricKey += BaseEncryption.encryptionPublic(symmetricKey,usrPublicKey)
		    userNames += i 
      	}
      	
      	var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication+"#$#$#$"+pageID+"#$#$#$"+userNames+"#$#$#$"+encrptedSymmetricKey+"#$#$#$"+encryptedMsg)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/PagePosting/"+username,entity))
        
        isPostSuccess onComplete {
          case Success(response) => println(response+": Page Post from:"+username+" to Page:"+pageID+" shared with: "+usrList)
          
        }
      }

      case AlbumPictureUpload(url:String, albumID:String, usrList:String) =>{
      	var userList = usrList+":"+username
      	var usr = userList.split(":")
      	var userNames = ""
      	var symmetricKey = BaseEncryption.generateSymmetricKey()
      	var encryptedMsg = BaseEncryption.encryptionSymmetric(url,symmetricKey)
      	var encrptedSymmetricKey = ""
      	for(i <- usr){
      		if(!userNames.equals("")){
      			userNames += "####"
      			encrptedSymmetricKey += "####"
      		}
      		var usrPublicKey = Global.publicKeys.getOrElse(i,null)
		    encrptedSymmetricKey += BaseEncryption.encryptionPublic(symmetricKey,usrPublicKey)
		    userNames += i 
      	}
      	
      	var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication+"#$#$#$"+albumID+"#$#$#$"+userNames+"#$#$#$"+encrptedSymmetricKey+"#$#$#$"+encryptedMsg)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/AlbumPictureUpload/"+username,entity))
        
        isPostSuccess onComplete {
          case Success(response) => println(response+": Picture Upload from:"+username+" to album:"+albumID+" shared with: "+usrList)
          
        }
      }

      case ProfilePictureUpload(url:String, usrID:String, usrList:String) =>{
      	var userList = usrList+":"+username
      	var usr = userList.split(":")
      	var userNames = ""
      	var flag:Boolean = false
      	var symmetricKey = BaseEncryption.generateSymmetricKey()
      	var encryptedMsg = BaseEncryption.encryptionSymmetric(url,symmetricKey)
      	var encrptedSymmetricKey = ""
      	for(i <- usr){
      		if(i.equals(usrID))
      			flag = true
      		if(!userNames.equals("")){
      			userNames += "####"
      			encrptedSymmetricKey += "####"
      		}
      		var usrPublicKey = Global.publicKeys.getOrElse(i,null)
		    encrptedSymmetricKey += BaseEncryption.encryptionPublic(symmetricKey,usrPublicKey)
		    userNames += i 
      	}

      	if(flag == false){
      		userNames += "####"
      		encrptedSymmetricKey += "####"
      		var usrPublicKey = Global.publicKeys.getOrElse(usrID,null)
		    encrptedSymmetricKey += BaseEncryption.encryptionPublic(symmetricKey,usrPublicKey)
		    userNames += usrID
      	}
      	
      	var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication+"#$#$#$"+usrID+"#$#$#$"+userNames+"#$#$#$"+encrptedSymmetricKey+"#$#$#$"+encryptedMsg)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/ProfilePictureUpload/"+username,entity))
        
        isPostSuccess onComplete {
          case Success(response) => println(response+": Picture Upload from:"+username+" to Profile of:"+usrID+" shared with: "+usrList)
        }
      }

      case ProfilePosting(msg:String, usrProfile:String,usrList:String) =>{
      	var userList = usrList+":"+username
      	var usr = userList.split(":")
      	var userNames = ""
      	var flag:Boolean = false
      	var symmetricKey = BaseEncryption.generateSymmetricKey()
      	var encryptedMsg = BaseEncryption.encryptionSymmetric(msg,symmetricKey)
      	var encrptedSymmetricKey = ""
      	for(i <- usr){
      		if(i.equals(usrProfile))
      			flag = true
      		if(!userNames.equals("")){
      			userNames += "####"
      			encrptedSymmetricKey += "####"
      		}
      		var usrPublicKey = Global.publicKeys.getOrElse(i,null)
		    encrptedSymmetricKey += BaseEncryption.encryptionPublic(symmetricKey,usrPublicKey)
		    userNames += i 
      	}
      	if(flag == false){
      		userNames += "####"
      		encrptedSymmetricKey += "####"
      		var usrPublicKey = Global.publicKeys.getOrElse(usrProfile,null)
		    encrptedSymmetricKey += BaseEncryption.encryptionPublic(symmetricKey,usrPublicKey)
		    userNames += usrProfile
      	}
      	
      	var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication+"#$#$#$"+usrProfile+"#$#$#$"+userNames+"#$#$#$"+encrptedSymmetricKey+"#$#$#$"+encryptedMsg)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/ProfilePosting/"+username,entity))
        
        isPostSuccess onComplete {
          case Success(response) => println(response+": Profile Post from:"+username+" to:"+usrProfile+" shared with: "+usrList)
          
        }
      }

      case GetPost(postID:String) => {
        var webClient = new SprayWebClientGet()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication+"#$#$#$"+postID)
        var hashResponse = webClient.get("http://127.0.0.1:8000/GetPost/"+username,entity)
        
        hashResponse onComplete {
          case Success(response) => {
          	if(response.equals("Access_Denied"))
          		println(response+" for user: "+ username+ " for post: "+postID)
          	else{
          		var responseArray = response.split("#\\$#\\$#\\$")
	          	var encryptedMsg = responseArray(0)
	          	var encrptedSymmetricKey = responseArray(1)
	          	var symmetricKey = BaseEncryption.decryptionPrivate(encrptedSymmetricKey,keyPair.getPrivate)
	          	var msg = BaseEncryption.decryptionSymmetric(encryptedMsg, symmetricKey)
	          	println("Success for retirieving post:"+postID+" for user:"+username+" Post="+msg)
          	}
          	
          }
        }
      }

      case GetPicture(pictureID:String) => {
        var webClient = new SprayWebClientGet()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication+"#$#$#$"+pictureID)
        var hashResponse = webClient.get("http://127.0.0.1:8000/GetPicture/"+username,entity)
        
        hashResponse onComplete {
          case Success(response) => {
          	if(response.equals("Access_Denied"))
          		println(response+" for user: "+ username+ " for picture: "+pictureID)
          	else{
          		var responseArray = response.split("#\\$#\\$#\\$")
	          	var encryptedMsg = responseArray(0)
	          	var encrptedSymmetricKey = responseArray(1)
	          	var symmetricKey = BaseEncryption.decryptionPrivate(encrptedSymmetricKey,keyPair.getPrivate)
	          	var msg = BaseEncryption.decryptionSymmetric(encryptedMsg, symmetricKey)
	          	println("Success for retirieving picture:"+pictureID+" for user:"+username+" Picture="+msg)
          	}
          	
          }
        }
      }

      case GetPublicKeys => {
      	
		var webClient = new SprayWebClientGet()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication)
        var hashResponse = webClient.get("http://127.0.0.1:8000/GetPublicKey/"+username,entity)
        hashResponse onComplete {
          case Success(response) => 
          	println("Public Keys Retrieved by user:"+username)
          	var keyArray = response.split("#\\$#\\$#\\$")
          	for(i <- keyArray){
          		var tempArray = i.split("####")
          		Global.publicKeys += (tempArray(0)->tempArray(1))
          	}
        }
	  } 

	  case CreatePage => {
	  	
	  	var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/CreatePage/"+username,entity))
        isPostSuccess onComplete {
          case Success(response) => println("New Page Created by user:"+username)
          
        }
	  }

	  case CreateAlbum => {
	  	
	  	var webClient = new SprayWebClientPost()(system)
        val entity = HttpEntity(ContentTypes.`text/plain`, tokenAuthentication)
        val isPostSuccess: Future[String]=webClient.pipeline(Post("http://127.0.0.1:8000/CreateAlbum/"+username,entity))
        isPostSuccess onComplete {
          case Success(response) => println("New Album Created by user:"+username)
          
        }
	  }
 
      
  }
}


 object Client extends App {
   var nrOfNodes = 10
   var argumentNumber = 0
   var actorCollection = new HashMap[String, ActorRef]()
   implicit val system = ActorSystem("Node")
   import system.dispatcher
   var webClient = new SprayWebClientGet()(system)

   for(i <- 0 to nrOfNodes-1){
            var workerName = "user%s".format(i)
            var worker = system.actorOf(Props(new ClientActor(system)), name = workerName)
            actorCollection += (workerName -> worker)
            println("Client Created: %s".format(workerName))
            worker ! Start
         
    }
    var user0 = actorCollection.getOrElse("user0",null)
	var user1 = actorCollection.getOrElse("user1",null)
	var user2 = actorCollection.getOrElse("user2",null)
	var user3 = actorCollection.getOrElse("user3",null)
	var user4 = actorCollection.getOrElse("user4",null)
	var user5 = actorCollection.getOrElse("user5",null)
	var user6 = actorCollection.getOrElse("user6",null)
	var user7 = actorCollection.getOrElse("user7",null)
	var user8 = actorCollection.getOrElse("user8",null)
	var user9 = actorCollection.getOrElse("user9",null)
	
	system.scheduler.scheduleOnce(2000 milliseconds,  user0, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user1, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user2, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user3, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user4, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user5, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user6, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user7, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user8, GetPublicKeys)
	system.scheduler.scheduleOnce(2000 milliseconds,  user9, GetPublicKeys)

	

	system.scheduler.scheduleOnce(3000 milliseconds,  user0, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user1, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user2, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user3, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user4, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user5, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user6, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user7, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user8, GetFriendsList)
	system.scheduler.scheduleOnce(3000 milliseconds,  user9, GetFriendsList)

	

	system.scheduler.scheduleOnce(8000 milliseconds,  user0, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user1, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user2, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user3, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user4, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user5, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user6, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user7, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user8, CreatePage)
	system.scheduler.scheduleOnce(8000 milliseconds,  user9, CreatePage)

	

	system.scheduler.scheduleOnce(13000 milliseconds,  user0, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user1, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user2, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user3, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user4, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user5, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user6, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user7, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user8, CreateAlbum)
	system.scheduler.scheduleOnce(13000 milliseconds,  user9, CreateAlbum)

	

	system.scheduler.scheduleOnce(18000 milliseconds,  user0, PagePosting("This_is_a_Post_Message_on_the_Page_of_Page_0_by_User0_Shared_With_User6_User9", "page0","user6:user9"))
	//system.scheduler.scheduleOnce(18000 milliseconds,  user5, PagePosting("This_is_a_Page_Post_for_Page_3_by_User5_Shared_With_User2_User3", "page3","user2:user3"))
	//system.scheduler.scheduleOnce(18000 milliseconds,  user9, PagePosting("This_is_a_Page_Post_for_Page_6_by_User9_Shared_With_User1_User4", "page6","user1:user4"))

	


	system.scheduler.scheduleOnce(23000 milliseconds,  user0, GetPost("post0"))
	system.scheduler.scheduleOnce(23000 milliseconds,  user5, GetPost("post0"))
	system.scheduler.scheduleOnce(23000 milliseconds,  user9, GetPost("post0"))
	system.scheduler.scheduleOnce(23000 milliseconds,  user6, GetPost("post0"))
	system.scheduler.scheduleOnce(23000 milliseconds,  user4, GetPost("post0"))

	

	system.scheduler.scheduleOnce(28000 milliseconds,  user0, ProfilePosting("This_is_a_Post_Message_on_Profile_of_User1_by_User0_Shared_with_User2_User3", "user1","user2:user3"))
	//system.scheduler.scheduleOnce(28000 milliseconds,  user0, ProfilePosting("This_is_a_Profile_Post_for_User1_by_User0_Shared_with_User2_User3", "user1","user2:user3"))
	//system.scheduler.scheduleOnce(28000 milliseconds,  user0, ProfilePosting("This_is_a_Profile_Post_for_User1_by_User0_Shared_with_User2_User3", "user1","user2:user3"))

	

	system.scheduler.scheduleOnce(33000 milliseconds,  user1, GetPost("post1"))
	system.scheduler.scheduleOnce(33000 milliseconds,  user0, GetPost("post1"))
	system.scheduler.scheduleOnce(33000 milliseconds,  user2, GetPost("post1"))
	system.scheduler.scheduleOnce(33000 milliseconds,  user3, GetPost("post1"))
	system.scheduler.scheduleOnce(33000 milliseconds,  user9, GetPost("post1"))

	

	system.scheduler.scheduleOnce(38000 milliseconds,  user0, AlbumPictureUpload("www.abc.com/album0/image.png", "album0","user2:user3"))
	
	

	system.scheduler.scheduleOnce(43000 milliseconds,  user0, GetPicture("picture0"))
	system.scheduler.scheduleOnce(43000 milliseconds,  user2, GetPicture("picture0"))
	system.scheduler.scheduleOnce(43000 milliseconds,  user3, GetPicture("picture0"))
	system.scheduler.scheduleOnce(43000 milliseconds,  user8, GetPicture("picture0"))

	

	system.scheduler.scheduleOnce(48000 milliseconds,  user8, ProfilePictureUpload("www.abc.com/profile2/image.png", "user2","user9:user3"))



	system.scheduler.scheduleOnce(53000 milliseconds,  user9, GetPicture("picture1"))
	system.scheduler.scheduleOnce(53000 milliseconds,  user2, GetPicture("picture1"))
	system.scheduler.scheduleOnce(53000 milliseconds,  user3, GetPicture("picture1"))
	system.scheduler.scheduleOnce(53000 milliseconds,  user8, GetPicture("picture1"))
	system.scheduler.scheduleOnce(53000 milliseconds,  user0, GetPicture("picture1"))


	
	
	/*system.scheduler.scheduleOnce(5000 milliseconds,  user0, PagePosting("This_is_a_Page_Post_for_Page_0", "page0","user2:user3"))
	system.scheduler.scheduleOnce(5000 milliseconds,  user0, ProfilePosting("This_is_a_Profile_Post_for_User_1", "user1","user2:user3"))
	system.scheduler.scheduleOnce(5000 milliseconds,  user0, ProfilePosting("This_is_a_Profile_Post_for_User_3", "user3","user2:user3"))
	system.scheduler.scheduleOnce(5000 milliseconds,  user0, AlbumPictureUpload("www.abc.com/album0/image.png", "album0","user2:user3"))
	system.scheduler.scheduleOnce(5000 milliseconds,  user0, AlbumPictureUpload("www.abc.com/album1/image.png", "album1","user2:user3"))
	system.scheduler.scheduleOnce(5000 milliseconds,  user0, ProfilePictureUpload("www.abc.com/profile2/image.png", "user2","user2:user3"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user1, GetPost("post0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user0, GetPost("post0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user2, GetPost("post0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user3, GetPost("post0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user1, GetPost("post1"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user0, GetPost("post1"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user2, GetPost("post1"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user3, GetPost("post1"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user0, GetPicture("picture0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user2, GetPicture("picture0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user3, GetPicture("picture0"))
	system.scheduler.scheduleOnce(10000 milliseconds,  user1, GetPicture("picture0"))*/
	
    
}

object Global {
 	var publicKeys = new HashMap[String, String]() 
	
  }

sealed trait FacebookSim
case object Start
case object GetFriendsList
case object ProfilePost
case object GetProfilePost
case object CreatePage
case object CreateAlbum
case object GetPage
case object GetAlbum
case object PagePost
case object AlbumPicture
case object UploadPicturetoProfile
case object GetPictures
case class ProfilePosting(msg:String, usrProfile:String, usr: String)
case class PagePosting(msg:String, pageID:String, usr:String)
case class GetPost(postId:String)
case object GetPublicKeys
case class AlbumPictureUpload(url:String, albumID:String, usrList:String)
case class ProfilePictureUpload(url:String, profileID:String, usrList:String)
case class GetPicture(pictureID:String)