package main.scala
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSelection.toScala
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable._
import scala.collection._
import scala.collection.convert.decorateAsScala._
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.TreeMap
import scala.util.Random
import scala.collection.mutable.HashMap
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.Set
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer



class MasterActor extends Actor
 {
 	var keyPair = BaseEncryption.keyPairGeneration
 	var publicKeyString = BaseEncryption.publicKeyToString(keyPair.getPublic)


    def populateFriendList(userList: HashMap[String, users]) = {  
      var stringList = ArrayBuffer[String]()
      var a = new ArrayBuffer[String]
      for(i<-0 to 9){
        a += "user"+i.toString
      }
      for(i<-0 to 9){

        a -= "user"+i.toString
        var temp = (scala.util.Random.shuffle(a.toList).take(2))
        Global.friendList += ("user"+i.toString->temp)      
        a += "user"+i.toString
      }
    }


  def receive = {
  	case Start() =>
  	{
        for(i<-0 to 9){
            var user = new users
            user.setUserId("user"+i.toString)
            Global.userList += ("user"+i.toString->user)                    
            
          }
          populateFriendList(Global.userList)
          sender ! "Users Created"
  	}

  	case Register(msg:String) =>
  	{
  		var msgArray = msg.split("#\\$#\\$#\\$")
  		Global.publicKeys += (msgArray(0)->msgArray(1))
	    var symmetricKey = BaseEncryption.generateSymmetricKey()
	    Global.authenticationToken += (msgArray(0)->symmetricKey)
      	var encrptedSymmetricKey = BaseEncryption.encryptionPublic(symmetricKey,msgArray(1))
    	var result = publicKeyString+"#$#$#$"+encrptedSymmetricKey
    	sender ! result
    }


    case GetFriendList(msg:String) =>
      {

      	
        var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
        	var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
        	if(checkingToken!=null)
	        	if(checkingToken.equals(decryptedToken)){
	        		result = ""+userObj.findFriendList
	        	}               
          
        }  
        
        sender ! result
    
      }
    case CreatePage(msg:String) =>
      {

      	var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
        	var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
        	if(checkingToken!=null)
	        	if(checkingToken.equals(decryptedToken)){
	        	      userObj.createPage(userID)
			          result = "Success" 
	        	}               
          
        }  
        
        sender ! result
    
      }

     case CreateAlbum(msg:String) =>
      {

      	var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
        	var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
        	if(checkingToken!=null)
	        	if(checkingToken.equals(decryptedToken)){
	        	      userObj.createAlbum(userID)
			          result = "Success" 
	        	}               
          
        }  
        
        sender ! result
    
      }
     
    case GetPost(msg: String) => 
    {

    	var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var postID = msgArray(2)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
        	var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
        	if(checkingToken!=null)
	        	if(checkingToken.equals(decryptedToken)){
	        		var hash = Global.postEncryptedKeys.getOrElse(postID,null)
    				if(hash!=null){

	    				var encryptedKey = hash.getOrElse(userID,null)
	    				if (encryptedKey!=null){
	    					var msg = Global.postList.getOrElse(postID,null)
	    					if(msg!=null){
	    						result = msg+"#$#$#$"+encryptedKey
	    					}
	    				}
	    		
	    			}
	        	}               
          
        }  
        
        sender ! result
    }

    case GetPicture(msg: String) => 
    {

    	var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var pictureID = msgArray(2)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
        	var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
        	if(checkingToken!=null)
	        	if(checkingToken.equals(decryptedToken)){
	        		var hash = Global.pictureEncryptedKeys.getOrElse(pictureID,null)
    				if(hash!=null){

	    				var encryptedKey = hash.getOrElse(userID,null)
	    				if (encryptedKey!=null){
	    					var msg = Global.pictureList.getOrElse(pictureID,null)
	    					if(msg!=null){
	    						result = msg+"#$#$#$"+encryptedKey
	    					}
	    				}
	    		
	    			}
	        	}               
          
        }  
        
        sender ! result
    }
    

    case PagePosting(msg:String) =>
    {
        var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
	        var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
	        if(checkingToken!=null)
                if(checkingToken.equals(decryptedToken)){
                    var pageID = msgArray(2)
                    var pageObj = Global.pageList.getOrElse(pageID, null)
                    if(pageObj!= null){
                    	var encryptedMsg = msgArray(5)
	                    var postID = "post"+Global.postID
	                    Global.postID += 1
	                    while(Global.postList.contains(postID)){
                            	postID = "post"+Global.postID
                            	Global.postID += 1
                            	
                        }
                        Global.postList += (postID -> encryptedMsg)
                        pageObj.posts += postID
	                    var users = msgArray(3)
	                    var userNames = users.split("####")
	                    var encrptedSymmetricKey = msgArray(4)
	                    var encrptedSymmetricKeyArray = encrptedSymmetricKey.split("####")
	                    var temp = new HashMap[String, String]()

	                    for(i<-0 to userNames.length-1){
	                            temp += (userNames(i) -> encrptedSymmetricKeyArray(i))
	                    }

	                    Global.postEncryptedKeys += (postID -> temp)
	                    result = "Success"
                    }
                    
                }
        }
        sender ! result
    }

    case ProfilePosting(msg:String) =>
    {
    	
        var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
                var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
                if(checkingToken!=null)
                if(checkingToken.equals(decryptedToken)){
                    var profileID = msgArray(2)
                    var profileObj = Global.userList.getOrElse(profileID, null)
                    if(profileObj!= null){
                    		
                    		var encryptedMsg = msgArray(5)

                            var postID = "post"+Global.postID
                            Global.postID += 1
                            while(Global.postList.contains(postID)){
                            	postID = "post"+Global.postID
                            	Global.postID += 1
                            	
                            }
                            Global.postList += (postID -> encryptedMsg)
                            profileObj.posts += postID
                            var users = msgArray(3)
                            var userNames = users.split("####")
                            var encrptedSymmetricKey = msgArray(4)
                            var encrptedSymmetricKeyArray = encrptedSymmetricKey.split("####")
                            var temp = new HashMap[String, String]()

                            for(i<-0 to userNames.length-1){
                                    temp += (userNames(i) -> encrptedSymmetricKeyArray(i))
                            }

                            
                            Global.postEncryptedKeys += (postID -> temp)
                            
                            result = "Success"
                    }

                }
        }
        sender ! result
    }

    case ProfilePictureUpload(msg:String) =>
    {
        var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
                var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
                if(checkingToken!=null)
                if(checkingToken.equals(decryptedToken)){
                    var profileID = msgArray(2)
                    var profileObj = Global.userList.getOrElse(profileID, null)
                    if(profileObj!= null){

                            var pictureID = "picture"+Global.pictureID
                            Global.pictureID += 1
                            while(Global.pictureList.contains(pictureID)){
                            	pictureID = "picture"+Global.pictureID
                            	Global.pictureID += 1
                            }
                            var encryptedMsg = msgArray(5)
                            Global.pictureList += (pictureID -> encryptedMsg)
                            profileObj.pictures += pictureID
                            var users = msgArray(3)
                            var userNames = users.split("####")
                            var encrptedSymmetricKey = msgArray(4)
                            var encrptedSymmetricKeyArray = encrptedSymmetricKey.split("####")
                            var temp = new HashMap[String, String]()

                            for(i<-0 to userNames.length-1){
                                    temp += (userNames(i) -> encrptedSymmetricKeyArray(i))
                            }

                            Global.pictureEncryptedKeys += (pictureID -> temp)
                            
                            
                            result = "Success"
                    }

                }
        }
        sender ! result
    }

    case AlbumPictureUpload(msg:String) =>
    {
        var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
	        var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
	        if(checkingToken!=null)
                if(checkingToken.equals(decryptedToken)){
                    var albumID = msgArray(2)
                    var albumObj = Global.albumList.getOrElse(albumID, null)
                    if(albumObj!= null){

	                    var pictureID = "picture"+Global.pictureID
	                    Global.pictureID += 1
	                    var encryptedMsg = msgArray(5)
	                    
	                    while(Global.pictureList.contains(pictureID)){
	                    	pictureID = "picture"+Global.pictureID
	                    	Global.pictureID += 1
	                    }
	                    Global.pictureList += (pictureID -> encryptedMsg)
	                    albumObj.pictures += pictureID
	                    var users = msgArray(3)
	                    var userNames = users.split("####")
	                    var encrptedSymmetricKey = msgArray(4)
	                    var encrptedSymmetricKeyArray = encrptedSymmetricKey.split("####")
	                    var temp = new HashMap[String, String]()

	                    for(i<-0 to userNames.length-1){
	                            temp += (userNames(i) -> encrptedSymmetricKeyArray(i))
	                    }

	                    Global.pictureEncryptedKeys += (pictureID -> temp)
	                    
	                    result = "Success"
                    }
                    
                }
        }
        sender ! result
    }

    case GetPublicKey(msg:String) =>
      {
        var result = "Access_Denied"
        var msgArray = msg.split("#\\$#\\$#\\$")
        var userID = msgArray(0)
        var tokenReceived = msgArray(1)
        var decryptedToken = BaseEncryption.decryptionPrivate(tokenReceived,keyPair.getPrivate)
        var userObj = Global.userList.getOrElse(userID,null)

        if(userObj!=null){
        	var checkingToken =  Global.authenticationToken.getOrElse(userID,null)
        	if(checkingToken!=null)
	        	if(checkingToken.equals(decryptedToken)){
	        		result = ""
	        		for((k,v) <- Global.publicKeys){
	        			if(!result.equals("")){
	        				result += "#$#$#$"
	        			}
	        			result += k+"####"+v  
	        		}

	        	}               
          
        }  
        
        sender ! result
    
      }
    
  }
}






class users{
      	var userID = ""

		var posts = new ArrayBuffer[String] with SynchronizedBuffer[String]
		var selfPostBuffer = new ArrayBuffer[String] with SynchronizedBuffer[String]
		var selfPagePostBuffer = new ArrayBuffer[String] with SynchronizedBuffer[String]

		var pictures = new ArrayBuffer[String] with SynchronizedBuffer[String]
		var selfPictureBuffer = new ArrayBuffer[String] with SynchronizedBuffer[String]
		var selfPagePictureBuffer = new ArrayBuffer[String] with SynchronizedBuffer[String]
		var selfAlbumPictureBuffer = new ArrayBuffer[String] with SynchronizedBuffer[String]
		def setUserId(id: String) = {
			userID = id
		}

		def getUserID: String = {
			return userID
		}

		def findFriendList : List[String] = {				
			return Global.friendList.getOrElse(userID, null)			
		}

		def selfPost(post:String, user:String) = {
			selfPostBuffer +=  post
		}

		def wallPost(post:String, userIDFrom:String) = {			
			posts += userIDFrom + ":" + post
		}

		def getWallPost : ArrayBuffer[String] = {
			return posts
		}

		def createPage(userID:String) = {
			
			var pageName = "page"+Global.pageID
			Global.pageID += 1
			var pageObj = new page
			pageObj.setPageID(pageName)
			
			pageObj.createdBy = userID

			var pages = new ListBuffer[String]
			pages = Global.userPages.getOrElse(userID,pages)
			pages += pageName
			Global.userPages += (userID -> pages)
			Global.pageList += (pageName -> pageObj)
		}

		def likePage(userID:String, pageID:String) = {
			var pageObj = Global.pageList.getOrElse(pageID,null)
			pageObj.likedBy += userID
		}

		def pagePost(userID:String, pageObj:page, post:String) = {
			pageObj.posts += userID + ":" + post
			selfPagePostBuffer +=  pageObj.pageID + ":" + post

		}

		def getPagesCreated(userID:String): ListBuffer[String]={
			return Global.userPages(userID)
		}

//____________________________________________________________________________

		def selfPicture(picture:String, user:String) = {
			selfPictureBuffer +=  picture
		}

		def wallPicture(picture:String, userIDFrom:String) = {			
			pictures += userIDFrom + ":" + picture
		}

		def getWallPicture : ArrayBuffer[String] = {
			return pictures
		}

		def createAlbum(userID:String) = {
			var albumName = "album"+Global.albumID
			Global.albumID += 1
			var albumObj = new album
			albumObj.setAlbumID(albumName)
			
			albumObj.createdBy = userID

			var albums = new ListBuffer[String]
			albums = Global.userAlbums.getOrElse(userID,albums)
			albums += albumName
			Global.userAlbums += (userID -> albums)
			Global.albumList += (albumName -> albumObj)
		}

		def likeAlbum(userID:String, albumID:String) = {
			var albumObj = Global.albumList.getOrElse(albumID,null)
			albumObj.likedBy += userID
		}

		def albumPicture(userID:String, albumObj:album, picture:String) = {
			albumObj.pictures += userID + ":" + picture
			selfAlbumPictureBuffer +=  albumObj.albumID + ":" + picture

		}

		def getAlbumsCreated(userID:String): ListBuffer[String]={
			return Global.userAlbums(userID)
		}

  }

 class page{
	var pageID = ""
	var likedBy = new ListBuffer[String]
	var posts = new ListBuffer[String]
	var pictures = new ListBuffer[String]
	var createdBy = ""

	def setPageID(setpageID: String) = {
		pageID = setpageID
	}

	def getPosts: ListBuffer[String] = {
		return posts
	}

	def getPictures: ListBuffer[String] = {
		return pictures
	}

	def getLikedBy: ListBuffer[String] = {
		return likedBy
	}

	def getCreatedBy: String = {
		return createdBy
	}
}
class album{
	var albumID = ""
	var likedBy = new ListBuffer[String]
	var pictures = new ListBuffer[String]
	var createdBy = ""

	def setAlbumID(setAlbumID: String) = {
		albumID = setAlbumID
	}

	def getPictures: ListBuffer[String] = {
		return pictures
	}

	def getLikedBy: ListBuffer[String] = {
		return likedBy
	}

	def getCreatedBy: String = {
		return createdBy
	}
}
 object Global {
 	var pageID = 0
 	var albumID = 0
 	var postID = 0
 	var pictureID = 0
    var friendList = new HashMap[String, List[String]]()
	var userPages = new HashMap[String, ListBuffer[String]]() //userID -> listOfPages
	var userAlbums = new HashMap[String, ListBuffer[String]]()
	var userList = new HashMap[String, users]() 
	var pageList = new HashMap[String, page]() 
	var albumList = new HashMap[String, album]()
	var postList = new HashMap[String, String]()
	var postEncryptedKeys =  new HashMap[String, HashMap[String, String]]()
	var pictureList = new HashMap[String, String]()
	var pictureEncryptedKeys =  new HashMap[String, HashMap[String, String]]()
	var publicKeys = new HashMap[String,String]
	var authenticationToken = new HashMap[String,String]
  }


sealed trait FacebookSim
case class GetFriendList(msg:String) extends FacebookSim
case class PostMsg(msg:String) extends FacebookSim
case class Start() extends FacebookSim
case class GetProfilePost(usrID:String) extends FacebookSim
case class CreatePage(msg:String) extends FacebookSim
case class GetPage(usrID:String) extends FacebookSim
case class PagePost(msg:String) extends FacebookSim
case class UploadPicturetoProfile(msg:String) extends FacebookSim
case class GetPictures(msg:String) extends FacebookSim
case class GetAlbum(msg:String) extends FacebookSim
case class CreateAlbum(msg:String) extends FacebookSim
case class AlbumPicture(msg:String) extends FacebookSim
case class Posting(msg:String) extends FacebookSim
case class GetPost(msg:String) extends FacebookSim
case class Register(msg:String) extends FacebookSim
case class GetPublicKey(msg:String) extends FacebookSim
case class PagePosting(msg:String) extends FacebookSim
case class ProfilePosting(msg:String) extends FacebookSim
case class AlbumPictureUpload(msg:String) extends FacebookSim
case class ProfilePictureUpload(msg:String) extends FacebookSim
case class GetPicture(pictureID:String) extends FacebookSim
