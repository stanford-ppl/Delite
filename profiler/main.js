
/*
	This is the top-level grid controller. It instantiates the various sub-views 
	such as code-editor, timeline, dataflow graph, bar charts, etc. All interactions 
	between these sub-views go through this grid controller. The sub-views do not 
	communicate with each other.
*/

var viewState = {}
viewState.highlightedGraphNode = -1
viewState.appSourceFileName = ""
viewState.degFile = ""
viewState.profileDataFile = ""
viewState.gcStatsFile = ""
viewState.highlightedLine = new aceRange(0,0,0,0)
viewState.globalStatsMetric = ""
viewState.selectedLevel = -1

var config = {}
config.markGraphNode = markGraphNode
config.markNeighborsOnGraph = markNeighborsOnGraph
config.highlightLineInEditor = highlightLineInEditor
config.highlightLineInEditorByKernelId = highlightLineInEditorByKernelId
config.populateKernelInfoTable = populateKernelInfoTable
config.populateKernelInfoTableById = populateKernelInfoTableById
config.populateSyncNodeInfoTable = populateSyncNodeInfoTable
config.displayGCEventStats = displayGCEventStats
config.enableNodeClickHandler = true // for bar-charts.
config.syncNodeRegex = /__sync-ExecutionThread-(\d+)-(.*)-(.*)-(\d+)$/
config.re_partition = /^(.*)_(\d+)$/
config.re_header = /^(.*)_h$/

addAppSourceFileHandler("srcDirInput", editor)
addDegFileHandler("degFileInput")
addProfileDataFileHandler("profDataInput")
addGCStatsFileHandler("gcStatsInput")

var editor = {}
var profData = {}
var graphController = {}
var timelineController = {}
var topNodesBasedOnTime = []
var topNodesBasedOnMemUsage = []
var threadLevelSyncStats = []

var getDisplayTextForTime = function(d) {
	var timeInMs = (d.time/1000).toFixed(0)
	var timeStr = " (" + timeInMs + "ms:" + d.percentage_time.toFixed(0) + "%)"
	return d.name + timeStr
}

var getDisplayTextForMemUsage = function(d) {
	return d.name + " (" + d.memUsage + "B)"
}

var getDisplayTextForThreadLevelSync = function(d) {
	return d.name + " (" + d.syncTimePct + "%)"
}

var getDisplayTextForRegionsData = function(d) {
	return d.name + " (" + d.percentage_time + "%)"
}

function onChangeRegionOption() {
	if (viewState.globalStatsMetric == "ticTocRegionStats") {
		var selectedRegionName = $(this).val()
		if (selectedRegionName == "__global") {
			displayOverallRegionsData()	
		} else { // display overall percentage data for regions
			var regions = profData.timelineData.ticTocRegions
			var selectedRegion = regions.filter(function(r) {return r.name == selectedRegionName})[0]
			displayKernelInfoForRegion(selectedRegion)
		}
	}
}

function onChangeDEGViewOption() {
	var view = $(this).val()
	graphController.changeColoringScheme(view)
}

$("#globalStatsMetric").change(function() {
	$("#generalInfo").hide()
	$("#dfgHeader").show()
	$("#dfg").show()

	var metric = $(this).val()
	if (metric == "performance") {
		clearDivForBarChartDisplay()
		createBarChart("#dfg", topNodesBasedOnTime, "percentage_time", getDisplayTextForTime, config)
	} else if (metric == "memUsage") {
		clearDivForBarChartDisplay()
		createBarChart("#dfg", topNodesBasedOnMemUsage, "memUsage", getDisplayTextForMemUsage, config)
	} else if (metric == "threadLevelSyncStats") {
		clearDivForBarChartDisplay()
		createBarChart("#dfg", threadLevelSyncStats, "syncTimePct", getDisplayTextForThreadLevelSync, config)
	} else if (metric == "ticTocRegionStats") {
		displayOverallRegionsData()
	} else if (metric == "degView") {
		$(".barChart").hide()
		$("#dfg").css("overflow-y", "hidden")
		$('.dataflowSvg').show()
		setUpSelectTagForDEGView()
		graphController.changeColoringScheme("dataDeps")
	}

	setGlobalStatsMetric(metric)
})

$('#timelineZoom').keyup(function(event){
    if(event.keyCode == 13){
        timelineController.zoom($(this).val() / 100)
    }
})

$('#searchKernel').keyup(function(event){
    if(event.keyCode == 13){
        searchNode($(this).val())
    }
})

$("#timelineLevelFilter").change(filterNodesOnTimeline)
$('#startButton').click(startDebugSession)

function setUpTimelineLevelFilter(maxNodeLevel) {
	var sel = $("#timelineLevelFilter")
	for (var i = 0; i <= maxNodeLevel; i++) {
		sel.append($("<option/>", {
			value: i,
			text: "Level " + i
		}))
	}
}

function setUpSelectTagForDEGView() {
	var sel = $("#globalViewOptions").empty()
	var views = ["dataDeps", "memUsage", "performance", "nodeType"]
	var viewLabels = ["Data Dependencies", "Memory Allocation", "Performance", "Type of Kernel"]
	$.each(views, function(i, v) {
		sel.append($("<option/>", {
			value: v,
			text: viewLabels[i]
		}))
	})

	$("#globalViewOptions").change(onChangeDEGViewOption)
}

function setUpSelectTagForRegions(regions) {
	var sel = $("#globalViewOptions").empty()
	sel.append('<option value="__global">All</option>')
	$.each(regions, function(i, r) {
		sel.append($("<option/>", {
        value: r.name,
        text: r.name
    }))
	})

	$("#globalViewOptions").change(onChangeRegionOption)
}

function displayOverallRegionsData() {
	clearDivForBarChartDisplay()
	var regions = profData.timelineData.ticTocRegions
	createBarChart("#dfg", regions, "percentage_time", getDisplayTextForRegionsData, config)
	setUpSelectTagForRegions(regions)
}

function displayKernelInfoForRegion(region) {
	function getDisplayText(d) {
		var timeInMs = (d.abs / 1000).toFixed(0)
		return d.name + " (" + d.abs + "ms : " + d.pct + "%)"
	}

	var childNodes = []
	var childToPerf = region.childToPerf
	for (k in region.childToPerf) {
		var o = {}
		o.name = k
		o.id = childToPerf[k].id
		o.abs = childToPerf[k].abs
		o.pct = childToPerf[k].pct
		childNodes.push(o)
	}

	childNodes.sort(function(a,b) {return b.pct - a.pct})
	clearDivForBarChartDisplay()
	createBarChart("#dfg", childNodes, "pct", getDisplayText, config)
}

function clearDivForBarChartDisplay() {
	$('.dataflowSvg').hide()
	$("#dfg").css("overflow-y", "auto")
	$(".barChart").remove()
}

function setGlobalStatsMetric(metric) {
	viewState.globalStatsMetric = metric
	$("#globalViewOptionsLabel").text($("#globalStatsMetric option:selected").text())

	if ((metric == "ticTocRegionStats") || (metric == "degView")) {
		$("#globalViewOptions").css("opacity", 1)
	} else {
		$("#globalViewOptions").css("opacity", 0)
	}
}

function filterNodesOnTimeline() {
	var selectedLevel = parseInt($("#timelineLevelFilter").val())
	for (var i = 0; i < 4; i++) {
		var filter = ".level-" + i
		if (i != selectedLevel) {
  			timelineController.hideNodes(filter)
		} else {
			timelineController.showNodes(filter)
		}
	}

	viewState.selectedLevel = selectedLevel
}

function markGraphNode(kernelId) {
	var n = viewState.highlightedGraphNode
	if (n != -1) {
		graphController.unhighlightNode(n)
	}

	graphController.highlightNode(kernelId)
	viewState.highlightedGraphNode = kernelId
}

function markNeighborsOnGraph(nodeId) {
	graphController.markNeighbors(nodeId)
}

function highlightLineInEditor(file, line) {
	unhighlightLine(viewState.highlightedLine)
	if (file in fileNameToFile) {
		if (file != viewState.appSourceFileName) readFile(file)
		viewState.highlightedLine = highlightLine(line)
		viewState.appSourceFileName = file
		$("#srcFileName").text(file)
	} else {
		console.log("WARNING: Selected kernel's sourceContext does not match the source file being viewed")
	}
}

function highlightLineInEditorByKernelId(nodeId) {
	var node = profData.dependencyData.nodes[nodeId]
	var sc = node.sourceContext
	highlightLineInEditor(sc.file, sc.line)
}

function populateKernelInfoTable(node) {
	function helper(num) {
		if (num) return num.toFixed(0)
		return "NA"
	} 

	function getEffectiveNodeType(n) {
		if (n.type == "InternalNode") {
			var parent = profData.dependencyData.nodes[n.parentId]
			return parent.type
		}

		return n.type
	}

	var nodeType = getEffectiveNodeType(node)
	var timeInMs = (node.time/1000).toFixed(0)
	var timeStr = timeInMs + "ms (" + node.percentage_time.toFixed(0) + "%)"
	var execTimePct = helper(node.execTime.pct)
	var syncTimePct = helper(node.syncTime.pct)
	var memUsage = node.memUsage + " B"
	var values = [node.name, nodeType, node.target, timeStr , execTimePct + "/" + syncTimePct + " %", memUsage]
	var table = $("#kernelInfoTable")[0]
	values.forEach(function(v, i) {
		var row = table.rows[i + 1]
		row.cells[1].innerHTML = values[i]
	})
}

function populateSyncNodeInfoTable(node) {
	var properties = ["Dep. Thread", "Dep. Kernel", "Time (%)"]
	var values = [node.dep_thread, node.dep_kernel, "NA"]
	var table = $("#syncNodeInfoTable")[0]
	properties.forEach(function(p, i) {
		var row = table.rows[i + 1]
		row.cells[1].innerHTML = values[i]
	})
}

function populateKernelInfoTableById(nodeId) {
	var node = profData.dependencyData.nodes[nodeId]
	populateKernelInfoTable(node)
}

function cloneNode(node, attrs) {
	var o = {}
	attrs.forEach(function(a) {o[a] = node[a]})

	return o
}

function getTopNodes(nodes, comparisonField, count) {
    var topNodes = []
    var relevantAttrs = ["id", "name", "time", comparisonField] // HACK: 'time' field need not be included when computing top nodes based on mem usage
    var clonedNodes = nodes.filter(function(n) {return n.type != "InternalNode"})
    			   		   .map(function(n) {return cloneNode(n, relevantAttrs)})
    clonedNodes.sort(function(n1,n2) {return n2[comparisonField] - n1[comparisonField]})

    if (count > clonedNodes.length) count = clonedNodes.length
    for (var i = 0; i < count; i++) {
        topNodes.push(clonedNodes[i])
    }

    return topNodes
}

function displayGCEventStats(data) {
	$("#dfg").hide()
	$("#dfgHeader").hide()
	$("#generalInfo").show()
	$("#infoTable").remove()

	createTable("infoTable", "#generalInfo", data)
}

function startDebugSession() {
	if ((viewState.degFile != "") && (viewState.profileDataFile != "")) {
		setGlobalStatsMetric($("#globalStatsMetric").val())
		setUpSelectTagForDEGView()

		editor = createEditor("code")
  		profData = getProfileData(degOps, profileData.Profile, config)
  		//setUpTimelineLevelFilter(profData.dependencyData.maxNodeLevel)
  		graphController = createDataFlowGraph(cola, "#dfg", profData.dependencyData, viewState, config)
  		//graphController = {}

  		// This is the data to be visualized using bar charts
  		topNodesBasedOnTime = getTopNodes(profData.dependencyData.nodes, "percentage_time", 20)
  		topNodesBasedOnMemUsage = getTopNodes(profData.dependencyData.nodes, "memUsage", 20)

  		threadLevelSyncStats = profData.threadLevelPerfStats.map(function(o, i) {return {
  			name: "T" + i,
  			syncTimePct: o.syncTime.pct
  		}})

      	//timelineController = createTimeline("#timeline", profData, config)
      	timelineController = new TimelineGraph("#timeline", profData, config)
      	timelineController.draw();
  		//$("#timelineHiddenNodeList").change(timelineController.timelineScopeSelHandler) 
  		//var f = function() {timelineController.timelineScopeSelHandler()}
  		//$("#timelineHiddenNodeList").change(f) 	
  		$("#timelineHiddenNodeList").change({
  			graph: timelineController
  		}, timelineController.timelineScopeSelHandler) 

      	//filterNodesOnTimeline()
      	createStackGraph("#memory", profData.memUsageData, timelineController.xScale)
      	createGCStatsGraph("#gcStats", gcEvents, timelineController.xScale, config)

		$(".timelineWrapper").on("scroll", function() {
			var tmp = $(".timelineWrapper")[0]
			$("#memory")[0].scrollLeft = tmp.scrollLeft
			$("#gcStats")[0].scrollLeft = tmp.scrollLeft
		})
    } else {
    	alert("Please upload the DEG file and the profile data (profData.js) and retry")
    }
}

function searchNode(nodeName) {
	var nodeId = profData.dependencyData.nodeNameToId[nodeName]
	var node = profData.dependencyData.nodes[nodeId]
	config.populateKernelInfoTable(node)

	var sc = node.sourceContext
	config.highlightLineInEditor(sc.file, sc.line)

	graphController.markNeighbors(nodeId)
}

$(document).ready(function () {
	$("#right").css("width", $("#container").width() - $("#accordian").width())
	$("#dfg").css("height", $("#dfg").height() - $("#dfgHeader").height())
	$(window).resize(function() {
		/*console.log("rigth: " + $("#right").height())
		console.log("code: " + $("#code").height())
		console.log("dfg: " + $("#dfg").height())
		console.log("timeline: " + $("#timeline").height())*/
	})
	/*
	$('#accordian > ul > li').click(function() {
		$("#accordian ul li").removeClass("active")
	    $(this).addClass("active")

	    var checkElement = $(this).next()
	    if ((checkElement.is("ul")) && (checkElement.is(":visible"))) {
	        return false
	    }
	    if ((checkElement.is('ul')) && (!checkElement.is(':visible'))) {
	        $("#accordian ul:visible").slideUp("normal")
	        checkElement.slideDown("normal")
	        return false
	    }
  	})
	*/
})