package ppl.delite.runtime.messages

import java.nio._
import java.nio.channels._
import java.nio.channels.spi._
import java.net._
import java.util.concurrent.{CountDownLatch, Executors}
import scala.collection.mutable.{ArrayBuffer, HashMap, SynchronizedMap, SynchronizedQueue, Queue}

case class ConnectionManagerId(val host: String, val port: Int) {

  val socketAddr: InetSocketAddress = {
    var address: InetAddress = null
    for (addr <- InetAddress.getAllByName(host) if address eq null) {
      //println("considering " + addr)
      if (addr.isReachable(10)) address = addr
    }
    if (address eq null) throw new RuntimeException("unable to establish local socket connection")
    new InetSocketAddress(address, port)
  }
  
  def toSocketAddress() = socketAddr
}

object ConnectionManagerId {
  def fromSocketAddress(socketAddress: InetSocketAddress): ConnectionManagerId = {
    new ConnectionManagerId(socketAddress.getHostName, socketAddress.getPort)
  }
}
  
class ConnectionManager(port: Int) {

  def this() = this(0)

  class MessageStatus(
      val message: Message,
      val connectionManagerId: ConnectionManagerId,
      completionHandler: MessageStatus => Unit) {

    var ackMessage: Option[Message] = None
    var attempted = false
    var acked = false

    def markDone() { completionHandler(this) }
  }
  
  val selector = SelectorProvider.provider.openSelector()
  val handleMessageExecutor = Executors.newFixedThreadPool(System.getProperty("spark.core.connection.handler.threads","20").toInt)
  val serverChannel = ServerSocketChannel.open()
  val connectionsByKey = new HashMap[SelectionKey, Connection] with SynchronizedMap[SelectionKey, Connection] 
  val connectionsById = new HashMap[ConnectionManagerId, SendingConnection] with SynchronizedMap[ConnectionManagerId, SendingConnection]
  val messageStatuses = new HashMap[Int, MessageStatus] 
  val connectionRequests = new HashMap[ConnectionManagerId, SendingConnection] with SynchronizedMap[ConnectionManagerId, SendingConnection]
  val keyInterestChangeRequests = new SynchronizedQueue[(SelectionKey, Int)]
  val sendMessageRequests = new Queue[(Message, SendingConnection)]
  
  var onReceiveCallback: (BufferMessage, ConnectionManagerId) => Option[Message]= null

  serverChannel.configureBlocking(false)
  serverChannel.socket.setReuseAddress(true)
  serverChannel.socket.setReceiveBufferSize(256 * 1024) 

  serverChannel.socket.bind(new InetSocketAddress(port))
  serverChannel.register(selector, SelectionKey.OP_ACCEPT)

  private def log(mssg: String) = println(mssg)
  private def log(mssg: String, e: Exception) = println(mssg)

  val id = ConnectionManagerId(InetAddress.getLocalHost.getHostName, serverChannel.socket.getLocalPort)
  log("Bound socket to port " + serverChannel.socket.getLocalPort() + " with id = " + id)
  
  val thisInstance = this
  val selectorThread = new Thread("connection-manager-thread") {
    override def run() {
      thisInstance.run()
    }
  }
  selectorThread.setDaemon(true)
  selectorThread.start()

  def run() {
    try {
      while(!selectorThread.isInterrupted) {
        for( (connectionManagerId, sendingConnection) <- connectionRequests) {
          sendingConnection.connect() 
          addConnection(sendingConnection)
          connectionRequests -= connectionManagerId
        }
        sendMessageRequests.synchronized {
          while(!sendMessageRequests.isEmpty) {
            val (message, connection) = sendMessageRequests.dequeue
            connection.send(message)
          }
        }

        while(!keyInterestChangeRequests.isEmpty) {
          val (key, ops) = keyInterestChangeRequests.dequeue
          val connection = connectionsByKey(key)
          val lastOps = key.interestOps()
          key.interestOps(ops)
          
          def intToOpStr(op: Int): String = {
            val opStrs = ArrayBuffer[String]()
            if ((op & SelectionKey.OP_READ) != 0) opStrs += "READ"
            if ((op & SelectionKey.OP_WRITE) != 0) opStrs += "WRITE"
            if ((op & SelectionKey.OP_CONNECT) != 0) opStrs += "CONNECT"
            if ((op & SelectionKey.OP_ACCEPT) != 0) opStrs += "ACCEPT"
            if (opStrs.size > 0) opStrs.reduceLeft(_ + " | " + _) else " "
          }
          
          log("Changed key for connection to [" + connection.remoteConnectionManagerId  + 
            "] changed from [" + intToOpStr(lastOps) + "] to [" + intToOpStr(ops) + "]")
          
        }

        val selectedKeysCount = selector.select()
        if (selectedKeysCount == 0) {
          log("Selector selected " + selectedKeysCount + " of " + selector.keys.size + " keys")
        }
        if (selectorThread.isInterrupted) {
          log("Selector thread was interrupted!")
          return
        }
        
        val selectedKeys = selector.selectedKeys().iterator()
        while (selectedKeys.hasNext()) {
          val key = selectedKeys.next
          selectedKeys.remove()
          if (key.isValid) {
            if (key.isAcceptable) {
              acceptConnection(key)
            } else 
            if (key.isConnectable) {
              connectionsByKey(key).asInstanceOf[SendingConnection].finishConnect()
            } else 
            if (key.isReadable) {
              connectionsByKey(key).read()
            } else 
            if (key.isWritable) {
              connectionsByKey(key).write()
            }
          }
        }
      }
    } catch {
      case e: Exception => log("Error in select loop", e)
    }
  }
  
  def acceptConnection(key: SelectionKey) {
    val serverChannel = key.channel.asInstanceOf[ServerSocketChannel]
    val newChannel = serverChannel.accept()
    val newConnection = new ReceivingConnection(newChannel, selector)
    newConnection.onReceive(receiveMessage)
    newConnection.onClose(removeConnection)
    addConnection(newConnection)
    log("Accepted connection from [" + newConnection.remoteAddress.getAddress + "]")
  }

  def addConnection(connection: Connection) {
    connectionsByKey += ((connection.key, connection))
    if (connection.isInstanceOf[SendingConnection]) {
      val sendingConnection = connection.asInstanceOf[SendingConnection]
      connectionsById += ((sendingConnection.remoteConnectionManagerId, sendingConnection))
    }
    connection.onKeyInterestChange(changeConnectionKeyInterest)
    connection.onException(handleConnectionError)
    connection.onClose(removeConnection)
  }

  def removeConnection(connection: Connection) {
    connectionsByKey -= connection.key
    if (connection.isInstanceOf[SendingConnection]) {
      val sendingConnection = connection.asInstanceOf[SendingConnection]
      val sendingConnectionManagerId = sendingConnection.remoteConnectionManagerId
      log("Removing SendingConnection to " + sendingConnectionManagerId)
      
      connectionsById -= sendingConnectionManagerId

      messageStatuses.synchronized {
        messageStatuses
          .values.filter(_.connectionManagerId == sendingConnectionManagerId).foreach(status => {
            log("Notifying " + status)
            status.synchronized {
            status.attempted = true 
             status.acked = false
             status.markDone()
            }
          })

        messageStatuses.retain((i, status) => { 
          status.connectionManagerId != sendingConnectionManagerId 
        })
      }
    } else if (connection.isInstanceOf[ReceivingConnection]) {
      val receivingConnection = connection.asInstanceOf[ReceivingConnection]
      val remoteConnectionManagerId = receivingConnection.remoteConnectionManagerId
      log("Removing ReceivingConnection to " + remoteConnectionManagerId)
      
      val sendingConnectionManagerId = connectionsById.keys.find(_.host == remoteConnectionManagerId.host).orNull
      if (sendingConnectionManagerId == null) {
        log("Corresponding SendingConnectionManagerId not found")
        return
      }
      log("Corresponding SendingConnectionManagerId is " + sendingConnectionManagerId)
      
      val sendingConnection = connectionsById(sendingConnectionManagerId)
      sendingConnection.close()
      connectionsById -= sendingConnectionManagerId
      
      messageStatuses.synchronized {
        for (s <- messageStatuses.values if s.connectionManagerId == sendingConnectionManagerId) {
          log("Notifying " + s)
          s.synchronized {
            s.attempted = true
            s.acked = false
            s.markDone()
          }
        }

        messageStatuses.retain((i, status) => { 
          status.connectionManagerId != sendingConnectionManagerId 
        })
      }
    }
  }

  def handleConnectionError(connection: Connection, e: Exception) {
    log("Handling connection error on connection to " + connection.remoteConnectionManagerId)
    removeConnection(connection)
  }

  def changeConnectionKeyInterest(connection: Connection, ops: Int) {
    keyInterestChangeRequests += ((connection.key, ops))  
  }

  def receiveMessage(connection: Connection, message: Message) {
    val connectionManagerId = ConnectionManagerId.fromSocketAddress(message.senderAddress)
    log("Received [" + message + "] from [" + connectionManagerId + "]") 
    val runnable = new Runnable() {
      val creationTime = System.currentTimeMillis
      def run() {
        log("Handler thread delay is " + (System.currentTimeMillis - creationTime) + " ms")
        handleMessage(connectionManagerId, message)
        log("Handling delay is " + (System.currentTimeMillis - creationTime) + " ms")
      }
    }
    handleMessageExecutor.execute(runnable)
    /*handleMessage(connection, message)*/
  }

  private def handleMessage(connectionManagerId: ConnectionManagerId, message: Message) {
    log("Handling [" + message + "] from [" + connectionManagerId + "]")
    message match {
      case bufferMessage: BufferMessage => {
        if (bufferMessage.hasAckId) {
          val sentMessageStatus = messageStatuses.synchronized {
            messageStatuses.get(bufferMessage.ackId) match {
              case Some(status) => { 
                messageStatuses -= bufferMessage.ackId 
                status
              }
              case None => { 
                throw new Exception("Could not find reference for received ack message " + message.id)
                null
              }
            }
          }
          sentMessageStatus.synchronized {
            sentMessageStatus.ackMessage = Some(message)
            sentMessageStatus.attempted = true
            sentMessageStatus.acked = true
            sentMessageStatus.markDone()
          }
        } else {
          val ackMessage = if (onReceiveCallback != null) {
            log("Calling back")
            onReceiveCallback(bufferMessage, connectionManagerId)
          } else {
            log("Not calling back as callback is null")
            None
          }
          
          if (ackMessage.isDefined) {
            if (!ackMessage.get.isInstanceOf[BufferMessage]) {
              log("Response to " + bufferMessage + " is not a buffer message, it is of type " + ackMessage.get.getClass())
            } else if (!ackMessage.get.asInstanceOf[BufferMessage].hasAckId) {
              log("Response to " + bufferMessage + " does not have ack id set")
              ackMessage.get.asInstanceOf[BufferMessage].ackId = bufferMessage.id
            }
          }

          sendMessage(connectionManagerId, ackMessage.getOrElse { 
            Message.createBufferMessage(bufferMessage.id)
          })
        }
      }
      case _ => throw new Exception("Unknown type message received")
    }
  }

  private def sendMessage(connectionManagerId: ConnectionManagerId, message: Message) {
    def startNewConnection(): SendingConnection = {
      val inetSocketAddress = connectionManagerId.toSocketAddress
      val newConnection = connectionRequests.getOrElseUpdate(connectionManagerId,
          new SendingConnection(inetSocketAddress, selector, connectionManagerId))
      newConnection   
    }
    val lookupKey = ConnectionManagerId.fromSocketAddress(connectionManagerId.toSocketAddress)
    val connection = connectionsById.getOrElse(lookupKey, startNewConnection())
    message.senderAddress = id.toSocketAddress()
    log("Sending [" + message + "] to [" + connectionManagerId + "]")
    /*connection.send(message)*/
    sendMessageRequests.synchronized {
      sendMessageRequests += ((message, connection))
    }
    selector.wakeup()
  }

  def sendMessageUnsafe(connectionManagerId: ConnectionManagerId, message: Message) {
    val status = new MessageStatus(message, connectionManagerId, s => ())
    messageStatuses.synchronized {
      messageStatuses += ((message.id, status))
    }
    sendMessage(connectionManagerId, message)
  }

  def sendMessageAsync(connectionManagerId: ConnectionManagerId, message: Message): Future[Option[Message]] = {
    val future = new Future[Option[Message]]
    val status = new MessageStatus(message, connectionManagerId, s => future.set(s.ackMessage))
    messageStatuses.synchronized {
      messageStatuses += ((message.id, status))
    }
    sendMessage(connectionManagerId, message)
    future
  }

  def sendMessageSync(connectionManagerId: ConnectionManagerId, message: Message): Option[Message] = {
    sendMessageAsync(connectionManagerId, message).get
  }

  def onReceiveMessage(callback: (Message, ConnectionManagerId) => Option[Message]) {
    onReceiveCallback = callback
  }

  def stop() {
    selectorThread.interrupt()
    selectorThread.join()
    selector.close()
    val connections = connectionsByKey.values
    connections.foreach(_.close())
    if (connectionsByKey.size != 0) {
      log("All connections not cleaned up")
    }
    handleMessageExecutor.shutdown()
    log("ConnectionManager stopped")
  }
}


object ConnectionManager {

  def main(args: Array[String]) {
  
    val manager = new ConnectionManager(9999)
    manager.onReceiveMessage((msg: Message, id: ConnectionManagerId) => { 
      println("Received [" + msg + "] from [" + id + "]")
      None
    })
    
    /*testSequentialSending(manager)*/
    /*System.gc()*/

    /*testParallelSending(manager)*/
    /*System.gc()*/
    
    /*testParallelDecreasingSending(manager)*/
    /*System.gc()*/

    testContinuousSending(manager)
    System.gc()
  }

  def testSequentialSending(manager: ConnectionManager) {
    println("--------------------------")
    println("Sequential Sending")
    println("--------------------------")
    val size = 10 * 1024 * 1024 
    val count = 10
    
    val buffer = ByteBuffer.allocate(size).put(Array.tabulate[Byte](size)(x => x.toByte))
    buffer.flip

    (0 until count).map(i => {
      val bufferMessage = Message.createBufferMessage(buffer.duplicate)
      manager.sendMessageUnsafe(manager.id, bufferMessage)
    })
    println("--------------------------")
    println()
  }

  def testParallelSending(manager: ConnectionManager) {
    println("--------------------------")
    println("Parallel Sending")
    println("--------------------------")
    val size = 10 * 1024 * 1024 
    val count = 10

    val buffer = ByteBuffer.allocate(size).put(Array.tabulate[Byte](size)(x => x.toByte))
    buffer.flip

    val startTime = System.currentTimeMillis
    (0 until count).map(i => {
      val bufferMessage = Message.createBufferMessage(buffer.duplicate)
      manager.sendMessageUnsafe(manager.id, bufferMessage)
    })
    val finishTime = System.currentTimeMillis
    
    val mb = size * count / 1024.0 / 1024.0
    val ms = finishTime - startTime
    val tput = mb * 1000.0 / ms
    println("--------------------------")
    println("Started at " + startTime + ", finished at " + finishTime) 
    println("Sent " + count + " messages of size " + size + " in " + ms + " ms (" + tput + " MB/s)")
    println("--------------------------")
    println()
  }

  def testParallelDecreasingSending(manager: ConnectionManager) {
    println("--------------------------")
    println("Parallel Decreasing Sending")
    println("--------------------------")
    val size = 10 * 1024 * 1024 
    val count = 10
    val buffers = Array.tabulate(count)(i => ByteBuffer.allocate(size * (i + 1)).put(Array.tabulate[Byte](size * (i + 1))(x => x.toByte)))
    buffers.foreach(_.flip)
    val mb = buffers.map(_.remaining).reduceLeft(_ + _) / 1024.0 / 1024.0

    val startTime = System.currentTimeMillis
    (0 until count).map(i => {
      val bufferMessage = Message.createBufferMessage(buffers(count - 1 - i).duplicate)
      manager.sendMessageUnsafe(manager.id, bufferMessage)
    })
    val finishTime = System.currentTimeMillis
    
    val ms = finishTime - startTime
    val tput = mb * 1000.0 / ms
    println("--------------------------")
    /*println("Started at " + startTime + ", finished at " + finishTime) */
    println("Sent " + mb + " MB in " + ms + " ms (" + tput + " MB/s)")
    println("--------------------------")
    println()
  }

  def testContinuousSending(manager: ConnectionManager) {
    println("--------------------------")
    println("Continuous Sending")
    println("--------------------------")
    val size = 10 * 1024 * 1024 
    val count = 10

    val buffer = ByteBuffer.allocate(size).put(Array.tabulate[Byte](size)(x => x.toByte))
    buffer.flip

    val startTime = System.currentTimeMillis
    while(true) {
      (0 until count).map(i => {
          val bufferMessage = Message.createBufferMessage(buffer.duplicate)
          manager.sendMessageUnsafe(manager.id, bufferMessage)
      })
      val finishTime = System.currentTimeMillis
      Thread.sleep(1000)
      val mb = size * count / 1024.0 / 1024.0
      val ms = finishTime - startTime
      val tput = mb * 1000.0 / ms
      println("Sent " + mb + " MB in " + ms + " ms (" + tput + " MB/s)")
      println("--------------------------")
      println()
    }
  }
}
