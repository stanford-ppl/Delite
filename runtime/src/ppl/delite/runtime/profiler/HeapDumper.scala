
package ppl.delite.runtime.profiler

import javax.management.MBeanServer;
import java.lang.management.ManagementFactory;
import com.sun.management.HotSpotDiagnosticMXBean;

// This code is based on the one provided at:
// https://blogs.oracle.com/sundararajan/entry/programmatically_dumping_heap_from_java

object HeapDumper {
    // This is the name of the HotSpot Diagnostic MBean
    private val HOTSPOT_BEAN_NAME = "com.sun.management:type=HotSpotDiagnostic";

    // field to store the hotspot diagnostic MBean 
    private var hotspotMBean: HotSpotDiagnosticMXBean = null;

    ///*
    def dumpHeap(fileName: String, live: Boolean) {
        // initialize hotspot diagnostic MBean
        initHotspotMBean();
        try {
            hotspotMBean.dumpHeap(fileName, live);
        } catch { 
            case re: RuntimeException => throw re;
            case exp: Exception => throw new RuntimeException(exp);
        }
    }

    // initialize the hotspot diagnostic MBean field
    def initHotspotMBean() {
        if (hotspotMBean == null) {
            //synchronized (HeapDumper.class) {
                if (hotspotMBean == null) {
                    hotspotMBean = getHotspotMBean();
                }
            //}
        }
    }

    // get the hotspot diagnostic MBean from the
    // platform MBean server
    def getHotspotMBean(): HotSpotDiagnosticMXBean = {
        try {
            val server = ManagementFactory.getPlatformMBeanServer();
            val bean = ManagementFactory.newPlatformMXBeanProxy(server, HOTSPOT_BEAN_NAME, classOf[HotSpotDiagnosticMXBean]);
            return bean;
        } catch {
            case re: RuntimeException => throw re;
            case exp: Exception => throw new RuntimeException(exp);
        }
    }

    /*
    public static void main(String[] args) {
        // default heap dump file name
        String fileName = "heap.bin";
        // by default dump only the live objects
        boolean live = true;

        // simple command line options
        switch (args.length) {
            case 2:
                live = args[1].equals("true");
            case 1:
                fileName = args[0];
        }

        // dump the heap
        dumpHeap(fileName, live);
    }
    //*/
}
