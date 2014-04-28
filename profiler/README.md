Delite debugger
================

The tool provides a platform to visualize the profile data collected from a Delite application, thereby making it easier to debug performanc issues in the app.

Using the tool
===============

Follow the steps below in order to generate the visualization

1.  Open grid.html in a Chrome browser (The tool has not been tested for other browsers)
2. On the siderbar located at the left, you would find a section named 'Inputs' at the top. Upload the DEG file and profile data file (named profile/profileData.js in the app's working directory) using the buttons provided. The src_dir input is optional. If provided, it would enable the tool to locate the source line corresponding to each kernel.
3. Once the files have been uploaded correctly, the 'View' button would turn green, indicating that the input files have been uploaded. Click the 'View' button' to start generating the visualization. Since the tool is in its initial version, it may take some time to generate the visualizations.

What does the tool visualize?
==============================

The main window of the tool is divided into 3 primary regions:
1. **Code-viewer** 
  * This is the window on the top left. 
  * When a kernel is selected, it displays the source file and the specific line of code that generated the kernel. 
  * Note that this feature would work only if the app's source directory has been uploaded [Refer to the section above on how to upload the source directory]

2. **Global data viewer** 
  * This is the window on the top right.
  * It supports the multiple views. Each view can be selected using the 'Metric' selection button provided on the sidebar under the section 'Global Stats'. Following are the available options:
    1. *Data Deps:* This generates a visualization of the DEG file. Nodes in the graph would be marked with 3 different colors depending on the in-degree and out-degree of the node.