Delite Performance Debugger
=============================

The tool provides a platform to visualize the profile data collected from a Delite application, thereby making it easier to debug performanc issues in a Delite app.

How do I profile my app?
=========================

* Use the --profile flag while invoking delitec/delite 

Using the tool
===============

Follow the steps below in order to generate the visualization

1. Open grid.html in a Chrome browser (The tool has not been tested for other browsers. Also, it would be advisable to update the Chrome browser to the latest version).
2. On the siderbar located at the left, you would find a section named 'Inputs' at the top. Upload the following files/dirs as inputs:
  * DEG file (eg: HelloWorld.deg)
  * profileData.js - Located within the 'profile' directory in the app's working directory. This directory and file are created when the app is run with the --profile flag enabled. 
  * gcStats.txt -  Located within the 'profile' directory in the app's working directory. It contains information about the garbage collection events that were triggered in the JVM during the execution of the app.
  * Source directory - Directory that contains the source code of your app. This input is optional. If provided, it would enable the tool to locate and highlight the source line corresponding to each kernel. 
3. Once the files have been uploaded correctly, the 'View' button would turn green, indicating that the input files have been uploaded. Click the 'View' button' to start generating the visualization. Since the tool is in a prototype stage, there are some performance issues that are yet to be resolved and hence, it may take up to a few minutes to generate the visualizations.

What does the tool visualize?
==============================

The tool's UI is composed of 3 tabbed sections:
  * Overview
  * Compare run summaries
  * Compare timelines

'Overview' section
===================

This is section is composed of 5 major views

1. **Code-viewer** 
  * This is the window on the top left. 
  * When a kernel is selected (either in the data-dependency or the timeline views), the corresponding source file is displayed here. Also, the specific line of code that generated the kernel is highlighted and scrolled into view.
  * NOTE: This feature would work only if the app's source directory has been uploaded [Refer to the section above on how to upload the source directory]

2. **Global data viewer** 
  * This is the window on the top right.
  * Supports multiple views. Each view can be selected using the 'Metric' selection button provided on the sidebar under the section 'Global Stats'. Following are the available options:
    1. *DEG View:* 
      * Generates a graph visualization of the DEG file. The edges represent data dependencies among the different kernels generated from the app. An arrow from node A to node B implies that B is data dependent on A.
      * Clicking on a node displays relevant information in the 'Kernel Info table' on the sidebar. Also, it highlights the input-output neighbors of the node on the graph. 
      * By default, each node in the graph are marked with one of 4 different colors depending on its in-degree and out-degree. 
        * Blue => in-degree == 0 AND out-degree == 0 
        * Green => in-degree == 0 AND out-degree > 0
        * Orange => in-degree > 0 AND out-degree > 0
        * Red => in-degree > 0 AND out-degree == 0
      * The nodes can be colored based on other metrics as well [These can be chosen using the drop-down selection on top of the 'Global data viewer']
        * Memory Allocation
          * The nodes are colored on a scale that ranged from white to red based on the memory allocated by each one. More the memory allocated by a node, the more red it is colored.
        * Performance
          * The nodes are colored on a scale that ranged from white to red based on the percentage of app time the node accounted for. More the time taken by the node, the more red it is.
        * Type of Kernel
          * Red => MultiLoop node
          * Orange => WhileLoop node
          * LightBlue => SingleTask node
          * Green => Conditional node
    2. *Performance:* 
      * Generates a bar chart that lists the top 20 kernels in terms of percentage of app time they account for. 
      * Each bar would display the name of the kernel along with the fraction of the total app runtime that it accounted for. 
      * Example: x0(10%) implies that the kernel 'x0' accounted for 10% of the total time taken by the app
    3. *Mem Usage:* 
      * Generates a bar chart that lists the top 20 kernels in terms of memory allocated. The amount of memory is indicated along with the kernel's name.
      * NOTE: Currently, memory allocation is tracked only for kernels run on the scala target. Other targets such as CUDA, C, etc. are not yet supported.
    4. *Thread Sync:* 
      * Generates a bar-chart that indicates the amount of time spent by each thread waiting on synchronization barriers. 
      * Example: T0(30%) implies that thread T0 spent almost 30% of its time waiting on synchronization barriers

3. **GC (Garbage Collection) Event Viewer**
  * This is the horizontal strip just below Code Viewer and the Global Data Viewer.
  * It illustrates the GC events that occurred during the execution of the app.
  * Each rectangle indicates a GC event. The width of the rectangle indicates the duration of the event.
  * Each rectangle is colored based on whether it represents a Major or Minor GC Event:
    * Purple => Minor GC
    * Red    => Major GC
  * To view more details about a GC event, click on the rectangle. This displays the following details about the event in the 'GC Event Info' table in lower part of the sidebar:
    * Mem: Type of memory - Young Generation, Old Generation or Heap
    * Before: Amount of memory in use before the GC event
    * After: Amount of memory in use after the GC event
    * Comm: Amount of memory that was reclaimed by the GC event

4. **JVM Memory Usage Viewer**
  * This is the horizontal strip below the GC Event Viewer
  * It illustrates the JVM memory usage during the execution of the app using a stack graph with 3 layers
    * The darkest blue layer indicates the maximum amount of memory that the JVM could possibly be allocated by the system.
    * The lighter blue layer indicates the total amount of memory that the JVM has been allocated by the system.
    * The lightest blue layer indicate the amount of JVM memory in use.
  * Note that the JVM memory usage stats are collected by means of sampling. While profiling, a background thread periodically queries the memory stats of the JVM. The orange markers on the graph indicate the samples that were collected. Clicking on one of the markers displays the memory usage stats at that timestamp in the 'Memory Usage Info' table in the sidebar.

5. **Timeline viewer**
  * This is the horizontal strip below the JVM Memory Usage Viewer
  * Provides a fine-grained view of the app's execution timeline at the level of threads. Each rectangle represents the execution of a kernel by a particular thread.
  * Each line corresponds to a different thread. If the app was run using 4 threads, there would 5 lines in the timeline view. The first 4 would correspond to the 4 threads and the last one illustrates the different tic-toc regions in the app.
  * Nodes are colored in the following manner:
    * Gray nodes
      * Represent the time spent waiting on a synchronization barrier.
      * Clicking on the node would display relevant information in the 'Sync Node Info' table in the sidebar. The field 'Dep. Kernel' indicates the kernel that the sync node is waiting for to complete and 'Dep. Thread' indicates the thread that's executing that kernel
    * Non-gray nodes
      * Represent the execution of a kernel. Clicking on a node displays relevant information in the 'Kernel Info table'
      * Note that the timeline nodes are arranged in a hierarchical manner. Double-click on a node to expand it and view the nodes that were executed within it. For example, double-clicking on a Conditional/WhileLoop/MultiLoop node would display the component nodes that constitute that node.
      * Each time, a timeline node is expanded by double-clicking, the 'Hidden Nodes' drop-down in the sidebar is updated. This drop-down can be used to move up to the parent node.

Other features in the Overview section
=========================================

* *Zoom:* Use the zoom box on the sidebar to zoom in and out of the timeline view

'Compare run summaries' section
=================================

* This section is intended to help in debugging scalability issues in a Delite app.
* In order to debug scalability issues, the programmer could run the app with varying numbers of threads (eg: 1,2,4,8, etc.) and then compare the variation of different metrics across runs.
* Currently, this section supports the comparison of the following metrics:
  * The variation in Total/Execution/Synchronization time across different tic-toc regions in the app with change in number of threads:
    * Total time is the amount of time taken by the kernel
    * Execution time is the amount of time spent by the kernel in computational (non-synchronization) tasks.
    * Synchronization time is the amount of time spent by the kernel in synchronization barriers - waiting for other threads to complete certain kernels.
  * The variation in Total/Execution/Synchronization time across different kernels in the app with change in number of threads.

* Using this view:
  * Use the 'Upload run summaries' button to upload multiple profileData.js files. 
  * Then click the 'View data' button to view the graphs that indicate the variation in different metrics for the tic-toc regions in the app.
  * Use the 'Metric' drop-down on the right to select the metric for which the variation needs to be visualized

  * The strip at the bottom can be used to view the same metrics for different kernels. 
  * Type in the name of the kernel (eg: x0, x254, etc.) in the search box and press the Enter key to display the data.

'Compare timelines' section
=================================

* This section is intended to help view timelines from multiple executions for comparison purposes.
* Use the 'Upload profile data' button to upload one profile data file at a time.

Known bugs and required features - Will be fixed/added soon
=============================================================

* In the 'Overview' section, when zooming into the timeline view, the GC Event timeline and the Memory Usage section do not zoom accordingly.

* In the 'Compare timelines' section, the different timeline views use different x-axis scales. And hence the widths of the kernels cannot be compared one-to-one.

* It's necessary to add a progress bar to indicate the status of backend processing to the user, especially after the input files have been uploaded. Else, its hard to tell if the tool has crashed or if its still processing data in the backend.
