//
//  Hello World client in Java
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//
package ppl.delite.runtime;

import java.nio.*;
import org.jeromq.ZMQ;
import java.util.StringTokenizer;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import ppl.delite.runtime.messages.Messages.*;
import ppl.delite.runtime.messages.*;
import ppl.delite.runtime.data.*;


public class Slave {
  public static volatile boolean finished = false;
  public static volatile Object stateLock = new Object();

  public static final byte OBJECT_SHELL_SIZE   = 8;
  public static final byte OBJREF_SIZE         = 4;
  public static final byte LONG_FIELD_SIZE     = 8;
  public static final byte INT_FIELD_SIZE      = 4;
  public static final byte SHORT_FIELD_SIZE    = 2;
  public static final byte CHAR_FIELD_SIZE     = 2;
  public static final byte BYTE_FIELD_SIZE     = 1;
  public static final byte BOOLEAN_FIELD_SIZE  = 1;
  public static final byte DOUBLE_FIELD_SIZE   = 8;
  public static final byte FLOAT_FIELD_SIZE    = 4;

  private static volatile int localID;
  private static volatile HashMap<Integer,ConnectionManagerId> directory;
  private static volatile int numSlaves;
  private static volatile LocalDeliteArrayDouble data;
  private static volatile int numRecieved = 0;
  private static volatile int numExpecting;
  private static volatile HashSet<Integer>[] pushSlavesMaster;
  private static volatile int[] localDegs;

  public static void waitForStateLock() {
    synchronized(stateLock){
      try {
        stateLock.wait();
      } catch (InterruptedException e) { }
    }
  }
  public static void notifyAllStateLock() {
    synchronized(stateLock){
      stateLock.notifyAll();
    }
  }
  public void init(int id, HashMap<Integer,ConnectionManagerId> hm,int numSlvs) {
    ZMQ.Context context = ZMQ.context(2);
    
    localID = id;
    directory = hm;
    numSlaves = numSlvs;

    DeliteMesosExecutor.sendDebugMessage("Kicking off zeroMQ");

    new Thread(new slave_slave_listener()).start();
    new Thread(new slave_slave_sender()).start();     

    System.out.println("Ending zeroMQ");

    context.term();
  }
  public void setGhostInfo(int numExpec, HashSet<Integer>[] psIn, int[] ld) {
    numExpecting = numExpec;
    pushSlavesMaster = psIn;
    localDegs = ld;
  }
  public void setGhostData(LocalDeliteArrayDouble dataIn) {
    data = dataIn;
    notifyAllStateLock();
  }
  //Listens for slave to slave data communication
  private static class slave_slave_listener implements Runnable {
    public void run() {
      ZMQ.Context context = ZMQ.context(1);
      ZMQ.Socket responder = context.socket(ZMQ.DEALER);

      //System.out.println("Getting Slave IDX: " + localID + " numSlaves: " + numSlaves + " hmSize: " + directory.size());

      ConnectionManagerId cm = directory.get(localID);
      String listener = "tcp://"  + cm.host() + ":" + cm.port();
      DeliteMesosExecutor.sendDebugMessage("Listening on: " + listener);
      responder.bind(listener);

      //Listener port is setup.
      DeliteMesosExecutor.sendDebugMessage("BINDING PORT IS FINISHED");
      try{
        Thread.sleep(1000);
      } catch(InterruptedException e){}
      DeliteMesosExecutor.doneInit_$eq(true);

      waitForStateLock();
      while (!Thread.currentThread().isInterrupted()) {
        byte[] request = responder.recv(0);
        unpackDynamicDataMessage(request,data.idMap(),data.ghostData());
        numRecieved++;
        if(numRecieved == numExpecting){
          DeliteMesosExecutor.donePushing_$eq(true); 
          numRecieved = 0;
          waitForStateLock();
        }
        //DeliteMesosExecutor.sendDebugMessage("\t NUMRECIEVED: " + numRecieved + " NUMEXPECTING: " + numExpecting);
      }

      responder.close();
      context.term();
    }
    public static void unpackDynamicDataMessage(byte[] data, HashMapIntIntImpl nd, double[] dat) {
      byte id = data[0];
      int length = ByteBuffer.wrap(data).getInt(BYTE_FIELD_SIZE);
      byte size = data[BYTE_FIELD_SIZE+INT_FIELD_SIZE];
      int index = ByteBuffer.wrap(data).getInt(BYTE_FIELD_SIZE+INT_FIELD_SIZE+BYTE_FIELD_SIZE);
      //System.out.println("dynamic id: " + id + " index: " + index + " length: " + length + " size: " + size);
      if(id == 0){
        for(int i=0;i<length;++i){
          //type dependent
          double cur = ByteBuffer.wrap(data).getDouble(BYTE_FIELD_SIZE+INT_FIELD_SIZE+BYTE_FIELD_SIZE+INT_FIELD_SIZE+i*(DOUBLE_FIELD_SIZE+size));
          //DeliteMesosExecutor.sendDebugMessage("RECIEVED INDEX: " + index + " DATA: " + cur);
          int keyIndx = nd.put(index);
          dat[keyIndx] = cur;
        }
      }
    }
  }

  private static class slave_slave_sender implements Runnable {
    public void run(){
      ZMQ.Context context = ZMQ.context(1);

      ZMQ.Socket[] requester = new ZMQ.Socket[numSlaves];

      //System.out.println("Slave IDX: " + localID + " numSlaves: " + numSlaves + " hmSize: " + directory.size());

      Iterator it = directory.entrySet().iterator();
      while (it.hasNext()) {
        Map.Entry<Integer,ConnectionManagerId> pairs = (Map.Entry<Integer,ConnectionManagerId>)it.next();
        int slvIdx = pairs.getKey();
        ConnectionManagerId id = pairs.getValue();
        String host = id.host();
        int port = id.port();
        //System.out.println("SlaveID: " + slvIdx + " host: " + host + ':' + port);

        if(slvIdx != -1 && slvIdx != localID){
          String name = "tcp://"  + host + ":" + port;
          //DeliteMesosExecutor.sendDebugMessage("\tSENDER INDEX: " + slvIdx + " name: " + name);
          requester[slvIdx] = context.socket(ZMQ.DEALER);
          requester[slvIdx].connect(name);
        } 
      }

      int numIterations = 0;
      while(!finished){
        waitForStateLock();

        pushData(pushSlavesMaster,requester);      

        //DeliteMesosExecutor.sendDebugMessage("ITERATION: " + numIterations);
        numIterations++;
        //data.idMap() = new HashMapIntIntImpl(data.length*5,data.length*5);
      }
      context.term();
    }
    public void pushData(HashSet<Integer>[] pushSlaves, ZMQ.Socket[] requester) { 
      //DeliteMesosExecutor.sendDebugMessage("\tPUSHING PRs");
      for(int i=0;i<data.length();i++){
        byte[] message = packDataMessage((byte)0,i+data.offset(),data.readAt(i+data.offset())/localDegs[i],DOUBLE_FIELD_SIZE);
        distributeDataMessage(pushSlaves[i],requester,message);
      }
    }
    public static void distributeDataMessage(HashSet<Integer> pushSlaves, ZMQ.Socket[] requester, byte[] message){
      Iterator iterator = pushSlaves.iterator();
      while(iterator.hasNext()){
        Integer cur = (Integer) iterator.next();
        //DeliteMesosExecutor.sendDebugMessage("\t Sending to slave ID: " + cur + " " + message.length);
        requester[cur].send(message, 0);
      }
    }
    public static <T> byte[] packDataMessage(byte id,int index,T data,byte size){
        // Display array elements
      final int length = 1;
      
      //id, length, size of prim, index, data
      byte[] message = new byte[BYTE_FIELD_SIZE+INT_FIELD_SIZE+BYTE_FIELD_SIZE+INT_FIELD_SIZE+length*size];
      packByte(message,0,id);
      packInt(message,BYTE_FIELD_SIZE,length);
      packByte(message,BYTE_FIELD_SIZE+INT_FIELD_SIZE,size);
      packInt(message,BYTE_FIELD_SIZE+INT_FIELD_SIZE+BYTE_FIELD_SIZE,index);

      //Set the data.
      switch(size){
        case INT_FIELD_SIZE:
          packInt(message,BYTE_FIELD_SIZE+INT_FIELD_SIZE+BYTE_FIELD_SIZE+INT_FIELD_SIZE,(Integer) data);
          break;
        case DOUBLE_FIELD_SIZE:
          packDouble(message,BYTE_FIELD_SIZE+INT_FIELD_SIZE+BYTE_FIELD_SIZE+INT_FIELD_SIZE,(Double) data);
        break;
      }

      return message;
    }
    public static void packByte(byte[] message, int index, byte data){
      message[index] = data;
    }
    public static void packInt(byte[] message, int index, int data){
      //copy length into message
      ByteBuffer bbuff = ByteBuffer.allocate(INT_FIELD_SIZE);
      bbuff.putInt(data); 
      byte[] b = bbuff.array();
      for(int i=0;i<b.length;i++){
        message[index+i] = b[i];
      }
    }
    public static void packInt(byte[] message, int index, int[] data){
      //copy length into message
      for(int i=0;i<data.length;i++){
        ByteBuffer bbuff = ByteBuffer.allocate(INT_FIELD_SIZE);
        bbuff.putInt(data[i]); 
        byte[] b = bbuff.array();
        for(int j=0;j<b.length;j++){
          message[index+i*INT_FIELD_SIZE+j] = b[j];
        }
      }  
    }
    public static <T> void packInt(byte[] message, int index, T[] data){
      //copy length into message
      for(int i=0;i<data.length;i++){
        ByteBuffer bbuff = ByteBuffer.allocate(INT_FIELD_SIZE);
        bbuff.putInt((Integer) data[i]); 
        byte[] b = bbuff.array();
        for(int j=0;j<b.length;j++){
          message[index+i*INT_FIELD_SIZE+j] = b[j];
        }
      }  
    }
    public static void packDouble(byte[] message, int index, double data){
      //copy length into message
      //DeliteMesosExecutor.sendDebugMessage("Sending index: " + index + " data: " + data);
      ByteBuffer bbuff = ByteBuffer.allocate(DOUBLE_FIELD_SIZE);
      bbuff.putDouble(data); 
      byte[] b = bbuff.array();
      for(int i=0;i<b.length;i++){
        message[index+i] = b[i];
      }
    }
  }
}