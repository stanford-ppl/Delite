Delite Performance Debugger
=============================

The tool provides a platform to visualize the profile data collected from a Delite application, thereby making it easier to debug performanc issues in a Delite app.

!['Overview' section](https://github.com/stanford-ppl/Delite/blob/debugger/profiler/assets/kMeans_2.png "'Overview' section of the profiler")

How do I profile my app?
=========================

* Use the --profile flag while invoking delitec/delite.
*  Example:
  * bin/delitec HelloSimpleCompiler --profile
  * bin/delite HelloSimpleCompiler --profile -t 4
* Additional memory access instrumentation can be enabled with argument(s) provided to the 'profile' flag.
  * --profile=pcm: Enables instrumentation of cache access stats (eg: cache hit/miss ratios, etc.) for Multiloop kernels.
    * NOTE: This option requires the Intel PCM library to be installed on the system.
  * --profile=perf: Enables data-structure-specific instrumentation of cache access stats.
    * NOTE: This option requires the 'perf' utility (installed by default on most Linux distributions)

Visualizing the collected profile info
=======================================

* Run bin/delitep from the same directory as bin/delitec and bin/delite were launched
* For the ‘delitep’ script to work, the following directory structure needs to be maintained:
  * apps/src/<app source files>
  * bin
    * delitep
    * server.py
  * profile
    * profile.db
    * profileDataUI.js
    * gcStats.txt


What does the tool visualize?
==============================

The tool's UI is composed of 2 tabbed sections:
  * Overview
  * Compare run summaries

Overview section
===================

This section is composed of 5 major views

1. **Code-viewer** 
  * This is the window on the top left. 
  * When a kernel is selected (either in the dataflow or the timeline views), the corresponding source file is displayed here. Also, the specific line of code that generated the kernel is highlighted and scrolled into view.

2. **Global data viewer** 
  * This is the window on the top right.
  * Generates a graph visualization of the DEG file. The edges represent data dependencies among the different kernels generated from the app. An arrow from node A to node B implies that B is data dependent on A.
  * Clicking on a node displays relevant information in the 'Kernel Info table' on the sidebar. Also, it highlights the input-output neighbors of the node on the graph. 
  * By default, each node in the graph is marked with one of 4 different colors depending on its in-degree and out-degree. 
	* *Blue* => (in-degree == 0) AND (out-degree == 0)
	* *Green* => (in-degree == 0) AND (out-degree > 0)
	* *Orange* => (in-degree > 0) AND (out-degree > 0)
	* *Red* => (in-degree > 0) AND (out-degree == 0)

3. **GC (Garbage Collection) Event Viewer**
  * This is the horizontal strip just below Code Viewer and the Global Data Viewer.
  * It illustrates the GC events that occurred during execution of the app.
  * Each rectangle indicates a GC event. The width of the rectangle indicates the duration of the event.
  * Each rectangle is colored based on whether it represents a Major or Minor GC Event:
    * *Purple* => Minor GC
    * *Red*    => Major GC
  * To view more details about a GC event, click on the rectangle. This displays the following details about the event in the 'GC Event Info' table (located in lower part of the sidebar)
    * *Mem:*     Type of memory (Young Generation, Old Generation or Heap)
    * *Before:*  Amount of memory in use before the GC event
    * *After:*   Amount of memory in use after the GC event
    * *Comm:*    Amount of memory that was reclaimed by the GC event

4. **JVM Memory Usage Viewer**
  * This is the horizontal strip below the GC Event Viewer
  * It illustrates the JVM memory usage during the execution of the app using a stack graph with 3 layers
    * The darkest blue layer indicates the maximum amount of memory that the JVM could possibly be allocated by the system.
    * The lighter blue layer indicates the total amount of memory that the JVM has been allocated by the system.
    * The lightest blue layer indicates the amount of JVM memory in use.
  * Note that the JVM memory usage stats are collected by means of sampling. While profiling, a background thread periodically queries the memory stats of the JVM. The orange markers on the graph indicate the samples that were collected. Clicking on one of the markers displays the memory usage stats (at the corresponding timestamp) in the 'Memory Usage Info' table (located in the sidebar).

5. **Timeline viewer**
  * This is the horizontal strip below the JVM Memory Usage Viewer
  * Provides a fine-grained view of the app's execution timeline at the level of threads. Each rectangle represents the execution of a kernel by a particular thread.
  * Each line corresponds to a different thread. If the app was run using 4 threads, there would 5 lines in the timeline view. The first 4 would correspond to the 4 threads and the last one illustrates the different tic-toc regions in the app.
  * Nodes are colored in the following manner:
    * Gray nodes
      * Represent the time spent waiting on a synchronization barrier.
      * Clicking on a node would display relevant information in the 'Sync Node Info' table (located in the sidebar). The field 'Dep. Kernel' indicates the kernel that the sync node is waiting for to complete and 'Dep. Thread' indicates the thread that's executing that kernel.
    * Non-gray nodes
      * Represent the execution of kernels. Clicking on a node displays relevant information in the 'Kernel Info table' (located in the sidebar).
      * Note that the timeline nodes are arranged in a hierarchical manner. Double-click on a node to expand it and to view the nodes that were executed within it. For example, double-clicking on a Conditional/WhileLoop/MultiLoop node would display the component nodes that constitute that node.
      * Each time, a timeline node is expanded by double-clicking, the 'Hidden Nodes' drop-down in the sidebar is updated. This drop-down can be used to move up to the parent node.

6. **Sidebar**
  * **Kernel Info table**
	* Displays information aggregated across all instances of a kernel.
      * *Search box:* Use this to view kernel info for a specific kernel
      * *Name:* Name of the kernel
      * *Type:* Type of the kernel (eg: SingleTask, MultiLoop, etc.)
      * *Time (%):* Total time taken by all instances of this kernel. Within parantheses, the percentage of total app time spent executing instances of this kernel is also provided.
	  * *Exec/Sync Time (%):* Percentage breakdown of the total time taken by the kernel in terms of time spent in computation and synchronization.
	  * *Mem Usage:* Total amount of memory allocated by the kernel
  * **Timeline Node Info**
	* Displays information corresponding to a specific instance of a kernel
	  * *Name:* Name of the kernel
	  * *Target:* Runtime target of the kernel instance (eg: Scala/Cpp/etc.)
	  * *Time (%):* Time (and percentage of total app time) taken by that particular instance of the kernel
	  * *Exec/Sync Time (%):* Percentage breakdown of the time spent by this instance of the kernel in terms of time spent in computation and synchronization
  * **Sync Node Info**
	* Displays information correspoding to a synchronization kernel
      * *Dep. Thread:* Name of the thread the synchronization waited on
	  * *Dep. Kernel:* Name of the kernel the synchronization waited on
	  * *Time (%):* Time (and percentage of total app time) taken by the synchronization kernel
  * **GC Event Info**
	* Displays information corresponding to the selected GC Event
	  * *Young Gen.:* Amount of memory that was used by Young Gen objects before and after the GC event. It also shows the amount of memory that was reclaimed from these objects.
	  * *Old Gen.:* Corresponds to Old Gen objects
	  * *Heap*
  * **Memory Usage Info**
	* Displays information corresponding to the selected point on the JVM Memory Usage Viewer.
	  * *M:* Max amount of memory that can be allocated to the JVM by the system
	  * *T:* Total amount of memory that has been allocated to the JVM
	  * *U:* Amount of memory in use by the JVM.

Other features in the Overview section
=========================================

* *Zoom:* Use the zoom box on the sidebar to zoom in and out of the timeline view
* For a zoom factor of F, enter (F*100) in the text box. For example, entering 300 zooms into the view by a factor of 3.

'Compare run summaries' section
=================================

* This section is intended to help in debugging scalability issues in a Delite app.
* In order to debug scalability issues, the programmer could run the app with varying numbers of threads (eg: 1,2,4,8, etc.) and then compare the variation of different metrics across runs.
* Currently, this section supports the comparison of the following metrics:
  * The variation in Total/Execution/Synchronization time across different tic-toc regions in the app with change in number of threads:
    * Total time is the amount of time taken by a tic-toc region.
    * Execution time is the amount of time spent by the region in computational (non-synchronization) tasks.
    * Synchronization time is the amount of time spent by the region in synchronization barriers - waiting for other threads to complete certain kernels.
  * The variation in Total/Execution/Synchronization time across different kernels in the app with change in number of threads.

* Using this view:
  * Use the 'Upload run summaries' button to upload multiple profile.db files. 
  * Once all the input files have been processed in the backend, the 'View data' button is enabled. Click this button to view the graphs that indicate the variation in different metrics for the tic-toc regions in the app.
  * Use the 'Metric' drop-down on the right to select the metric for which the variation needs to be visualized
  * The strip at the bottom can be used to view the same metrics for different kernels. 
  * Type in the name of the kernel (eg: x0, x254, etc.) in the search box and press the Enter key to display the data.
