Delite debugger
================

The tool provides a platform to visualize the profile data collected from a Delite application, thereby making it easier to debug performanc issues in the app.

How do I profile my app?
=========================

* Use the --profile flag while invoking delitec/delite 

Using the tool
===============

Follow the steps below in order to generate the visualization

1. Open grid.html in a Chrome browser (The tool has not been tested for other browsers)
2. On the siderbar located at the left, you would find a section named 'Inputs' at the top. Upload the DEG file and profile data file (named profile/profileData.js in the app's working directory) using the buttons provided. The src_dir input is optional. If provided, it would enable the tool to locate the source line corresponding to each kernel.
3. Once the files have been uploaded correctly, the 'View' button would turn green, indicating that the input files have been uploaded. Click the 'View' button' to start generating the visualization. Since the tool is in its initial version, it may take some time to generate the visualizations.

What does the tool visualize?
==============================

The main window of the tool is divided into 3 primary regions:

1. **Code-viewer** 
  * This is the window on the top left. 
  * When a kernel is selected, it displays the source file and the specific line of code that generated the kernel. 
  * NOTE: This feature would work only if the app's source directory has been uploaded [Refer to the section above on how to upload the source directory]

2. **Global data viewer** 
  * This is the window on the top right.
  * Supports multiple views. Each view can be selected using the 'Metric' selection button provided on the sidebar under the section 'Global Stats'. Following are the available options:
    1. *Data Deps:* 
      * Generates a visualization of the DEG file. 
      * Clicking on a node displays relevant information in the 'Kernel Info table' on the sidebar. Also, it highlights the input-output neighbors of the node on the graph. 
      * Nodes in the graph would be marked with 4 different colors depending on the in-degree and out-degree of the node in terms of data (true) dependencies. 
        * Blue: in-degree == 0 AND out-degree == 0 
        * Green: in-degree == 0 AND out-degree > 0
        * Orange: in-degree > 0 AND out-degree > 0
        * Red: in-degree > 0 AND out-degree == 0
    2. *Exec Time:* 
      * Generates a bar chart that lists the top 20 kernels in terms of time taken. 
      * Each bar would display the name of the kernel along with the fraction of the total app runtime that it accounted for. 
      * Example: x0(10%) implies that the kernel 'x0' accounted for 10% of the total time taken by the app
    3. *Mem Usage:* 
      * Generates a bar chart that lists the top 20 kernels in terms of memory allocated. The amount of memory is indicated in bytes(B).
      * NOTE: Currently, memory allocation is tracked only for kernels run on the scala target. Other targets such as CUDA, C, etc. are not supported
    4. *Thread Sync:* 
      * Generates a bar-chart that indicates the amount of time spent by each thread waiting on synchronization barriers. 
      * Example: T0(30%) implies that thread T0 spent almost 30% of its time waiting on synchronization barriers

3. **Timeline viewer**
  * This is the window at the bottom.
  * Provides a fine-grained view of the app's execution timeline at the level of threads. Each rectangle represents the execution of a kernel by a particular thread.
  * Each line corresponds to a different thread. If the app was run using 4 threads, there would 5 lines in the timeline view. The first 4 would correspond to the 4 threads and the last one would merely illustrate the overall app execution.
  * Nodes are colored in the following manner:
    * Gray nodes
      * Represent the time spent waiting on a synchronization barrier.
      * Clicking on the node would display relevant information in 'Sync Node Info' section on the sidebar. The field 'Dep. Kernel' indicates the kernel that the sync node is waiting for to complete and 'Dep. Thread' indicates the thread that's executing the kernel
    * Non-gray nodes: Represent the execution of a kernel. Clicking on a node displays relevant information in the 'Kernel Info table'

Other features
==============

* *Level Filter:* This can be used to drill down into the timeline nodes that have nested kernels within them. For example, a WhileLoop node would have conditional and body op kernels within it. Selecting Level 1 would break down each WhileLoop timeline node into its component nodes.
* *Zoom:* Use the zoom box on the sidebar to zoom in and out of the timeline view
