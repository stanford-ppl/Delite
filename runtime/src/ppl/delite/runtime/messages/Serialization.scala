package ppl.delite.runtime.messages

import com.google.protobuf.ByteString
import java.nio.ByteBuffer
import ppl.delite.runtime.data._
import ppl.delite.runtime.messages.Messages._
import ppl.delite.runtime.DeliteMesosExecutor


object Serialization {

  private var idOffset = 0
  private def nextOffset = {
    idOffset += 1
    (idOffset-1)
  }

  //FIXME: stack is not thread-safe
  private var sendId = false
  private var lookupId = false
  var symId: String = _

  //TODO: Move this to somewhere else. 
  //This is only used to check if a new array data is sent from the master so that GPU can update its stale copy.  
  var updated: Boolean = false

  private var copyLength = -1
  private var copyOffset = 0

  def serialize[T](value: T): ByteString = serialize(value, false, null, 0, -1)
  def serialize[T](value: T, sendId: Boolean, symId: String): ByteString = serialize(value, sendId, symId, 0, -1)
  def serialize[T](value: T, offset: Int, length: Int): ByteString = serialize(value, false, null, offset, length)

  def serialize[T](value: T, sendId: Boolean, symId: String, offset: Int, length: Int): ByteString = {
    this.sendId = sendId //initialize stack
    copyLength = length
    copyOffset = offset
    this.symId = symId
    idOffset = 0

    value match {
      case v:Int => IntMessage.newBuilder.setValue(v).build.toByteString
      case v:Long => LongMessage.newBuilder.setValue(v).build.toByteString
      case v:Float => FloatMessage.newBuilder.setValue(v).build.toByteString
      case v:Double => DoubleMessage.newBuilder.setValue(v).build.toByteString
      case v:Boolean => BooleanMessage.newBuilder.setValue(v).build.toByteString
      case v:Char => UIntMessage.newBuilder.setValue(v).build.toByteString
      case v:Byte => UIntMessage.newBuilder.setValue(v).build.toByteString
      case v:Short => IntMessage.newBuilder.setValue(v).build.toByteString
      case s:String => StringMessage.newBuilder.setValue(s).build.toByteString
      case a:DeliteArray[_] => serializeDeliteArray(a).toByteString
      case a: Array[Int] => serializeDeliteArray(new LocalDeliteArrayInt(a)).toByteString
      case a: Array[Char] => serializeDeliteArray(new LocalDeliteArrayChar(a)).toByteString
      case a: Array[Double] => serializeDeliteArray(new LocalDeliteArrayDouble(a)).toByteString
      case a: Array[Any] => serializeDeliteArray(new LocalDeliteArrayObject(a)).toByteString
      case other =>
        try {
          other.getClass.getMethod("toByteString").invoke(other).asInstanceOf[ByteString]
        }
        catch {
          case e: NoSuchMethodException => throw new UnsupportedOperationException("don't know how to serialize " + other.getClass.getSimpleName)
        }
    }
  }

  //TODO: for large arrays we probably want to avoid protobufs (extra copies)
  //seems to require 2 different interfaces? (ByteBuffer vs ByteString)
  def serializeDeliteArray(array: DeliteArray[_]): ArrayMessage = {
    val length = if (copyLength == -1) array.length else copyLength
    //DeliteMesosExecutor.sendDebugMessage("produced array of length: " + length)
    val offset = this.copyOffset
    array match {
      case a:DeliteArray[_] if sendId => 
        val symOffset = nextOffset
        saveLocally(symId, symOffset, a)
        val id = symId + "_" + symOffset
        ArrayMessage.newBuilder.setId(Id.newBuilder.setId(id)).setLength(length).build
      case a:RemoteDeliteArray[_] =>
        val mssg = ArrayMessage.newBuilder.setId(Id.newBuilder.setId(a.id))
        for (len <- a.offsets) mssg.addOffset(len)
        mssg.build
      case a:LocalDeliteArrayInt =>
        val buf = ByteBuffer.allocate(length*4)
        buf.asIntBuffer.put(a.data, offset, length)
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayLong => 
        val buf = ByteBuffer.allocate(length*8)
        buf.asLongBuffer.put(a.data, offset, length)
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayFloat => 
        val buf = ByteBuffer.allocate(length*4)
        buf.asFloatBuffer.put(a.data, offset, length)
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayDouble => 
        //ppl.delite.runtime.DeliteMesosExecutor.sendDebugMessage(a.data.mkString("local array: [ ", " ", " ]"))
        val buf = ByteBuffer.allocate(length*8)
        buf.asDoubleBuffer.put(a.data, offset, length)
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayChar => 
        val buf = ByteBuffer.allocate(length*2)
        buf.asCharBuffer.put(a.data, offset, length)
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayShort => 
        val buf = ByteBuffer.allocate(length*2)
        buf.asShortBuffer.put(a.data, offset, length)
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayBoolean => 
        val arr = a.data
        val buf = ByteBuffer.allocate(length)
        var i = offset
        while (i < offset+length) {
          val x: Byte = if (arr(i)) -1 else 0
          buf.put(x)
          i += 1
        }
        val array = ByteString.copyFrom(buf)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayByte => 
        val array = ByteString.copyFrom(a.data, offset, length)
        ArrayMessage.newBuilder.setLength(length).setArray(array).build
      case a:LocalDeliteArrayObject[_] =>
        val arr = a.data
        var i = offset
        
        val serialize = try {
          arr(i).getClass.getMethod("toByteString")
        }
        catch {
          case e: NoSuchMethodException => throw new UnsupportedOperationException("don't know how to serialize " + arr(i).getClass.getSimpleName)
        }

        val saveOffset = copyOffset
        copyOffset = 0
        val saveLength = copyLength
        copyLength = -1

        val mssg = ArrayMessage.newBuilder.setLength(length)
        while (i < offset+length) {
          mssg.addObject(serialize.invoke(arr(i)).asInstanceOf[ByteString])         
          i += 1
        }

        copyOffset = saveOffset
        copyLength = saveLength

        mssg.build
    }
  }

  //def deserialize[T](tpe: Class[T], bytes: ByteString, lookupId: Boolean = false): T = deserialize(Seq(tpe), bytes, lookupId)

  def deserialize[T](tpe: Class[T], bytes: ByteString, lookupId: Boolean = false): T = {
    this.lookupId = lookupId
    val res = tpe match {
      case tpe if tpe == classOf[Int] => IntMessage.parseFrom(bytes).getValue
      case tpe if tpe == classOf[Long] => LongMessage.parseFrom(bytes).getValue
      case tpe if tpe == classOf[Float] => FloatMessage.parseFrom(bytes).getValue
      case tpe if tpe == classOf[Double] => DoubleMessage.parseFrom(bytes).getValue
      case tpe if tpe == classOf[Char] => UIntMessage.parseFrom(bytes).getValue.asInstanceOf[Char]
      case tpe if tpe == classOf[Boolean] => BooleanMessage.parseFrom(bytes).getValue
      case tpe if tpe == classOf[Byte] => UIntMessage.parseFrom(bytes).getValue.asInstanceOf[Byte]
      case tpe if tpe == classOf[Short] => IntMessage.parseFrom(bytes).getValue.asInstanceOf[Short]
      case tpe if tpe == classOf[String] => StringMessage.parseFrom(bytes).getValue
      case tpe if tpe == classOf[DeliteArrayInt] => deserializeDeliteArrayInt(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayLong] => deserializeDeliteArrayLong(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayFloat] => deserializeDeliteArrayFloat(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayDouble] => deserializeDeliteArrayDouble(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayChar] => deserializeDeliteArrayChar(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayShort] => deserializeDeliteArrayShort(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayByte] => deserializeDeliteArrayByte(ArrayMessage.parseFrom(bytes))
      case tpe if tpe == classOf[DeliteArrayBoolean] => deserializeDeliteArrayBoolean(ArrayMessage.parseFrom(bytes))
      //case tp if tp == classOf[DeliteArrayObject[_]] => deserializeDeliteArrayObject(tpe(1), ArrayMessage.parseFrom(bytes))
      case other =>
        try {
          other.getMethod("parseFrom", classOf[ByteString]).invoke(null, bytes)
        }
        catch {
          case e: NoSuchMethodException => throw new UnsupportedOperationException("don't know how to deserialize " + other.getSimpleName)
        }
    }
    res.asInstanceOf[T]
  }

  def deserializeDeliteArrayInt(mssg: ArrayMessage): DeliteArrayInt = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Int](byteBuffer.capacity / 4)
      byteBuffer.asIntBuffer.get(array) 
      updated = true
      new LocalDeliteArrayInt(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayInt]
      else new RemoteDeliteArrayInt(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayLong(mssg: ArrayMessage): DeliteArrayLong = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Long](byteBuffer.capacity / 8)
      byteBuffer.asLongBuffer.get(array) 
      updated = true
      new LocalDeliteArrayLong(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayLong]
      else new RemoteDeliteArrayLong(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayFloat(mssg: ArrayMessage): DeliteArrayFloat = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Float](byteBuffer.capacity / 4)
      byteBuffer.asFloatBuffer.get(array) 
      updated = true
      new LocalDeliteArrayFloat(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayFloat]
      else new RemoteDeliteArrayFloat(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayDouble(mssg: ArrayMessage): DeliteArrayDouble = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Double](byteBuffer.capacity / 8)
      byteBuffer.asDoubleBuffer.get(array)
      //ppl.delite.runtime.DeliteMesosExecutor.sendDebugMessage(array.mkString("remote: [ "," ", " ]"))
      updated = true
      new LocalDeliteArrayDouble(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayDouble]
      else new RemoteDeliteArrayDouble(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayChar(mssg: ArrayMessage): DeliteArrayChar = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Char](byteBuffer.capacity / 2)
      byteBuffer.asCharBuffer.get(array) 
      updated = true
      new LocalDeliteArrayChar(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayChar]
      else new RemoteDeliteArrayChar(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayShort(mssg: ArrayMessage): DeliteArrayShort = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Short](byteBuffer.capacity / 2)
      byteBuffer.asShortBuffer.get(array) 
      updated = true
      new LocalDeliteArrayShort(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayShort]
      else new RemoteDeliteArrayShort(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayByte(mssg: ArrayMessage): DeliteArrayByte = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Byte](byteBuffer.capacity)
      byteBuffer.get(array) 
      updated = true
      new LocalDeliteArrayByte(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayByte]
      else new RemoteDeliteArrayByte(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayBoolean(mssg: ArrayMessage): DeliteArrayBoolean = {
    if (mssg.hasArray) {
      val byteBuffer = mssg.getArray.asReadOnlyByteBuffer
      val array = new Array[Boolean](byteBuffer.capacity)
      var i = 0
      while (i < array.length) {
        array(i) = byteBuffer.get != 0
        i += 1
      }
      updated = true
      new LocalDeliteArrayBoolean(array)
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayBoolean]
      else new RemoteDeliteArrayBoolean(id, Array(mssg.getLength))
    }
  }

  def deserializeDeliteArrayObject[T:Manifest](mssg: ArrayMessage): DeliteArrayObject[T] = {
    if (mssg.getObjectCount > 0) {
      val deserialize = try {
        manifest[T].erasure.getMethod("parseFrom", classOf[ByteString])
      }
      catch {
        case e: NoSuchMethodException => throw new UnsupportedOperationException("don't know how to deserialize " + manifest[T].erasure.getSimpleName)
      }
      val array = new Array[T](mssg.getLength)
      var i = 0
      while (i < array.length) {
        array(i) = deserialize.invoke(null, mssg.getObject(i)).asInstanceOf[T]
        i += 1
      }
      updated = true
      new LocalDeliteArrayObject[T](array)    
    }
    else {
      val id = mssg.getId.getId
      if (lookupId) lookupLocally(id, mssg.getOffsetList).asInstanceOf[DeliteArrayObject[T]]
      else new RemoteDeliteArrayObject[T](id, Array(mssg.getLength))
    }
  }

  //TODO: reorganize
  private def saveLocally[T](id: String, offset: Int, a: DeliteArray[T]) {
    if (offset == 0) ppl.delite.runtime.DeliteMesosExecutor.results.put(id, new java.util.ArrayList[DeliteArray[_]])
    ppl.delite.runtime.DeliteMesosExecutor.results.get(id).add(a)
  }

  private def lookupLocally[T](id: String, offsetList: java.util.List[Integer]): DeliteArray[T] = {
    val splitId = id.split("_")
    val symId = splitId(0)
    val offset = splitId(1).toInt
    val res = ppl.delite.runtime.DeliteMesosExecutor.getResult(symId, offset).asInstanceOf[DeliteArray[T]]
    val offsets = new Array[Int](offsetList.size)
    for (i <- 0 until offsets.length) offsets(i) = offsetList.get(i)
    res.offsets = offsets
    res.offset = offsets(ppl.delite.runtime.DeliteMesosExecutor.slaveIdx)
    res.id = id
    res
  }

}
