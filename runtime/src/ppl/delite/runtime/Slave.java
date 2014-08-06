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

public class Slave {
  private static volatile int localID;
  private static volatile HashMap<Integer,ConnectionManagerId> directory;
  private static volatile int numSlaves;

  private static volatile HashMapIntIntImpl recieverHashMap;

  public void init(int id, HashMap<Integer,ConnectionManagerId> hm,int numSlvs) {
    ZMQ.Context context = ZMQ.context(2);
    
    localID = id;
    directory = hm;
    numSlaves = numSlvs;

    System.out.println("Kicking off zeroMQ");

    new Thread(new slave_slave_listener()).start();
    new Thread(new slave_slave_sender()).start();     

    System.out.println("Ending zeroMQ");

    context.term();
  }

  //Listens for slave to slave data communication
  private static class slave_slave_listener implements Runnable {
    public void run() {
      ZMQ.Context context = ZMQ.context(1);
      ZMQ.Socket responder = context.socket(ZMQ.DEALER);

      System.out.println("Getting Slave IDX: " + localID + " numSlaves: " + numSlaves + " hmSize: " + directory.size());

      ConnectionManagerId cm = directory.get(localID);
      String listener = "tcp://"  + cm.host() + ":" + cm.port();
      System.out.println("Listening on: " + listener);
      responder.bind(listener);

      while (!Thread.currentThread().isInterrupted()) {
        byte[] request = responder.recv(0);
        //unpackDynamicDataMessage(request,recieverHashMap,g.remotePRs);
      }

      responder.close();
      context.term();
    }
  }
  private static class slave_slave_sender implements Runnable {
    public void run(){
      ZMQ.Context context = ZMQ.context(1);

      ZMQ.Socket[] requester = new ZMQ.Socket[numSlaves];

      System.out.println("Slave IDX: " + localID + " numSlaves: " + numSlaves + " hmSize: " + directory.size());

      Iterator it = directory.entrySet().iterator();
      while (it.hasNext()) {
        Map.Entry<Integer,ConnectionManagerId> pairs = (Map.Entry<Integer,ConnectionManagerId>)it.next();
        int slvIdx = pairs.getKey();
        ConnectionManagerId id = pairs.getValue();
        String host = id.host();
        int port = id.port();
        System.out.println("SlaveID: " + slvIdx + " host: " + host + ':' + port);

        if(slvIdx != -1 && slvIdx != localID){
          String name = "tcp://"  + host + ":" + port;
          System.out.println("Connecting to SlaveID: " + slvIdx + " name: " + name);
          requester[slvIdx] = context.socket(ZMQ.DEALER);
          requester[slvIdx].connect(name);
        } 
      }
      context.term();
    }
  }
}