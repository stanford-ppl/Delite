#ifndef _DELITE_CONFIG_
#define _DELITE_CONFIG_

#include <iostream>
#include <jni.h>
#include <mutex>         
#include "Config.h"

#ifdef __DELITE_CPP_NUMA__
#include <numa.h>
#endif


Config* config = 0;
std::mutex init_mtx;

// heavy-handed, but doesn't appear there is another good way
int getCpuInfo(FILE* pipe) {
    if (!pipe) return -1;

    char buffer[128];
    std::string result = "";
    while(!feof(pipe)) {
        if(fgets(buffer, 128, pipe) != NULL)
            result += buffer;
    }
    pclose(pipe);

    try {	
        int info = std::stoi(result);
        return info;
    }
    catch(std::exception const &e) {
	   return -1;
    }
}

int getNumCoresPerSocket() {
    FILE* pipe = popen("grep 'cores' /proc/cpuinfo 2> /dev/null | head -1 | cut -d ':' -f 2 | tr -d ' '", "r");
    return getCpuInfo(pipe);
}

int getNumSockets() {
    FILE* pipe = popen("grep 'physical id' /proc/cpuinfo 2> /dev/null | tail -1 | cut -d ':' -f 2 | tr -d ' '", "r");
    return getCpuInfo(pipe);
}

void initializeConfig(int numThreads) {
    config = new Config(numThreads);
    
    //detect physical config
    int numSockets = getNumSockets() + 1;
    int numCoresPerSocket = getNumCoresPerSocket();
    int numCores = numCoresPerSocket * numSockets;

    //detect numa config
    #ifdef __DELITE_CPP_NUMA__
    if (numa_available() >= 0) {
        int numCpus = numa_num_configured_cpus();
        if (numCoresPerSocket <= 0) {
            printf("[WARNING]: Unable to automatically determine number of physical cores, assuming %d\n", numCpus);
            numCores = numCpus;
        }

        int numNodes = numa_num_configured_nodes();
        if (numSockets > 0 && numSockets != numNodes) {
            printf("[WARNING]: Found %d sockets but %d NUMA nodes. Using %d nodes\n", numSockets, numNodes, numNodes);
        }
        numSockets = numNodes;
        numCoresPerSocket = numCores / numSockets; //potentially re-distribute cores across nodes
    }
    #endif

    if (numSockets > 0 && numCoresPerSocket > 0) {
        config->numSockets = numSockets;
        config->numCoresPerSocket = numCoresPerSocket;
        printf("[delite]: Detected machine configuration of %d socket(s) with %d core(s) per socket.\n", config->numSockets, config->numCoresPerSocket);
    }
    else {
        printf("[WARNING]: Unable to automatically detect machine configuration.  Assuming %d socket(s) with %d core(s) per socket.\n", config->numSockets, config->numCoresPerSocket);
    }
}


extern "C" JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_AccExecutionThread_initializeThread(JNIEnv* env, jobject obj, jint threadId, jint numThreads);

JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_AccExecutionThread_initializeThread(JNIEnv* env, jobject obj, jint threadId, jint numThreads) {
    init_mtx.lock();
    if (!config) {
        initializeConfig(numThreads);
    }
    init_mtx.unlock();

    #ifdef __DELITE_CPP_NUMA__
    if (numa_available() >= 0) {
        int socketId = config->threadToSocket(threadId);
        if (socketId < numa_num_configured_nodes()) {
            printf("[delite]: Binding thread %d to cpu %d, socket %d\n", threadId, threadId, socketId);
            bitmask* cpumask = numa_allocate_cpumask();
            numa_bitmask_setbit(cpumask, threadId);
            numa_sched_setaffinity(0, cpumask);
            bitmask* nodemask = numa_allocate_nodemask();
            numa_bitmask_setbit(nodemask, socketId);
            numa_set_membind(nodemask);
            //numa_bind(nodemask); //this is more lenient than setaffinity but doesn't appear to work robustly
        }
    }
    #endif
}

#endif
