
/*
	This is the top-level grid controller. It instantiates the various sub-views 
	such as code-editor, timeline, dataflow graph, bar charts, etc. All interactions 
	between these sub-views go through this grid controller. The sub-views do not 
	communicate with each other.
*/

var viewState = {};
viewState.highlightedGraphNode = -1;
viewState.appSourceFileName = "";
viewState.degFile = "";
viewState.profileDataFile = "";
viewState.gcStatsFile = "";
viewState.highlightedLine = new aceRange(0,0,0,0);
viewState.globalStatsMetric = "";
viewState.selectedLevel = -1;

var config = {};
config.markGraphNode = markGraphNode;
config.markNeighborsOnGraph = markNeighborsOnGraph;
config.highlightLineInEditor = highlightLineInEditor;
config.highlightLineInEditorByKernelId = highlightLineInEditorByKernelId;
config.populateGCEventInfoTable = populateGCEventInfoTable;
config.populateKernelInfoTable = populateKernelInfoTable;
config.populateKernelInfoTableById = populateKernelInfoTableById;
config.populateSyncNodeInfoTable = populateSyncNodeInfoTable;
config.enableNodeClickHandler = true; // for bar-charts.
config.syncNodeRegex = /__sync-ExecutionThread-(\d+)-(.*)-(.*)-(\d+)$/;
config.re_partition = /^(.*)_(\d+)$/;
config.re_header = /^(.*)_h$/;

addAppSourceFileHandler("srcDirInput", editor);
addDegFileHandler("degFileInput");
addProfileDataFileHandler("profDataInput");
addGCStatsFileHandler("gcStatsInput");

var editor = {}
var profData = {}
var graphController = {}
var timelineController = {}
var topNodesBasedOnTime = []
var topNodesBasedOnMemUsage = []
var threadLevelSyncStats = []


$("#globalStatsMetric").change(function() {
	$("#generalInfo").hide();
	$("#dfgHeader").show();
	$("#dfg").show();

	var metric = $(this).val();
	if (metric == "performance") {
		clearDivForBarChartDisplay();
		createBarChart("#dfg", topNodesBasedOnTime, "totalTimePct", getDisplayTextForTime, config);
	} else if (metric == "memUsage") {
		clearDivForBarChartDisplay();
		createBarChart("#dfg", topNodesBasedOnMemUsage, "memUsage", getDisplayTextForMemUsage, config);
	} else if (metric == "threadLevelSyncStats") {
		clearDivForBarChartDisplay();
		createBarChart("#dfg", threadLevelSyncStats, "syncTimePct", getDisplayTextForThreadLevelSync, config);
	} else if (metric == "ticTocRegionStats") {
		displayOverallRegionsData();
	} else if (metric == "degView") {
		$(".barChart").hide();
		$("#dfg").css("overflow-y", "hidden");
		$('.dataflowSvg').show();
		setUpSelectTagForDEGView();
		graphController.changeColoringScheme("dataDeps");
	}

	setGlobalStatsMetric(metric);
});

$('#timelineZoom').keyup(function(event){
    if(event.keyCode == 13){
        timelineController.zoom($(this).val() / 100);
    }
});

$('#searchKernel').keyup(function(event){
    if(event.keyCode == 13){
        searchNode($(this).val())
    }
});

$("#timelineLevelFilter").change(filterNodesOnTimeline);
$('#startButton').click(startDebugSession);

$(window).resize(function() {
	$("#right").css("width", $("#container").width() - $("#accordian").width());
	//$("#panel-1").css("height", $(window).height());
});

$(document).ready(function() {
	$("#right").css("width", $("#container").width() - $("#accordian").width());
	$("#dfg").css("height", $("#dfg").height() - $("#dfgHeader").height());
});

function getDisplayTextForTime(d) {
	return d.name + " (" + getDisplayTextForTimeAbsPctPair(d.totalTimeAbs, d.totalTimePct) + ")";
}

function getDisplayTextForMemUsage(d) {
	return d.name + " (" + memUsageValueToStr(d.memUsage) + ")";
}

function getDisplayTextForThreadLevelSync(d) {
	return d.name + " (" + d.syncTimePct + "%)"
}

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

	document.getElementById('globalViewOptions').onchange = onChangeDEGViewOption;
}

function onChangeDEGViewOption() {
	var view = $(this).val()
	graphController.changeColoringScheme(view)
}

function displayOverallRegionsData() {
	clearDivForBarChartDisplay();
	var regionDataPoints = profData.executionProfile.ticTocRegions.map(function(region) { return {
		"name" : region.name,
		"totalTimeAbs" : region.totalTime.abs,
		"totalTimePct" : region.totalTime.pct
	}});

	regionDataPoints.sort(function(p1,p2) { return p2.totalTimePct - p1.totalTimePct; });
	createBarChart("#dfg", regionDataPoints, "totalTimePct", getDisplayTextForTime, config);
	setUpSelectTagForRegions(regionDataPoints);
}

function setUpSelectTagForRegions(regions) {
	var sel = $("#globalViewOptions").empty();
	sel.append('<option value="__global">All</option>');
	$.each(regions, function(i, r) {
		sel.append($("<option/>", {
	        value: r.name,
	        text: "Region '" + r.name + "'"
    	}))
	});

	document.getElementById('globalViewOptions').onchange = onChangeRegionOption;
}

function onChangeRegionOption() {
	function getDisplayTextForRegionComp(d) {
		return d.name + " (" + getDisplayTextForTimeAbsPctPair(d.abs, d.pct) + ")";
	}

	if (viewState.globalStatsMetric == "ticTocRegionStats") {
		var selectedRegionName = $(this).val()
		if (selectedRegionName == "__global") {
			displayOverallRegionsData()	
		} else { // display overall percentage data for regions
			var regions = profData.executionProfile.ticTocRegions
			var selectedRegion = regions.filter(function(r) {return r.name == selectedRegionName})[0]
			var topNodesBasedOnTime = getTopNodesFromTicTocRegionBasedOnTime(selectedRegion, 20);
			clearDivForBarChartDisplay();
			createBarChart("#dfg", topNodesBasedOnTime, "pct", getDisplayTextForRegionComp, config);
		}
	}
}

function getTopNodesFromTicTocRegionBasedOnTime(region, N) {
	var childNodes = [];
	var childToPerf = region.childToPerf;
	for (name in region.childToPerf) {
		childNodes.push({
			"name" : name,
			"abs"  : childToPerf[name].abs,
			"pct"  : childToPerf[name].pct
		})
	}

	childNodes.sort(function(a,b) {return b.pct - a.pct});
	if (childNodes.length < N) {
		return childNodes;
	}

	return childNodes.slice(0, N);
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

function computeTarget(supportedTargets, enabledTargets) {
	if ( supportedTargets[TARGET_CUDA] && enabledTargets[TARGET_CUDA] ) {
		return "cuda";
	} else if ( supportedTargets[TARGET_CPP] && enabledTargets[TARGET_CPP] ) {
		return "cpp";
	}

	return "scala";
}

function populateKernelInfoTable(node) {
	function helper(num) {
		if (num != undefined) return num.toFixed(0)
		return "NA"
	};

	var dNode = (node.node) ? node.node : node;
	var nodeType = dNode.type;
	var target = computeTarget(dNode.supportedTargets, profData.executionProfile.enabledTargets);
	var values = [];
	var summary = profData.executionProfile.nodeNameToSummary[node.name];
	if (summary) {
		var timeStr = getDisplayTextForTimeAbsPctPair(summary.totalTime.abs, summary.totalTime.pct);
		var execTimePct = helper(summary.execTime.pct);
		var syncTimePct = helper(summary.syncTime.pct);
		var memUsage = memUsageValueToStr(summary.memUsage);
		values = [node.name, nodeType, target, timeStr , execTimePct + "/" + syncTimePct + " %", memUsage];
	} else {
		values = [node.name, nodeType, target, "-" , "-", "-"];
	}
	
	var table = $("#kernelInfoTable")[0];
	values.forEach(function(v, i) {
		var row = table.rows[i + 1];
		row.cells[1].innerHTML = values[i];
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

function getTopNodesBasedOnTotalTime(nodeNameToSummary, dependencyData, count) {
	var nodeNameAttrPairs = [];
	for (var name in nodeNameToSummary) {
		if (!isPartitionNode(name, config)) {
			var dNode = getDNodeCorrespondingToTNode(name, dependencyData, config);
			if (dNode && (dNode.level == 0)) {
				var totalTime = nodeNameToSummary[name].totalTime;
				nodeNameAttrPairs.push({
					"name": name, 
					"totalTimeAbs": totalTime.abs, 
					"totalTimePct": totalTime.pct,
					"node": dNode
				});
			}
		}
	}

	nodeNameAttrPairs.sort(function(p1,p2) { return p2.totalTimePct - p1.totalTimePct; });

	return nodeNameAttrPairs.slice(0,count);
}

function getTopNodesBasedOnMemUsage(nodeNameToSummary, dependencyData, count) {
	var nodeNameAttrPairs = [];
	for (var name in nodeNameToSummary) {
		if (!isPartitionNode(name, config)) {
			var dNode = getDNodeCorrespondingToTNode(name, dependencyData, config);
			if (dNode && (dNode.level == 0)) {
				nodeNameAttrPairs.push({
					"name": name, 
					"memUsage": nodeNameToSummary[name].memUsage,
					"node": dNode
				});
			}
		}
	}

	nodeNameAttrPairs.sort(function(p1,p2) { return p2.memUsage - p1.memUsage; });

	return nodeNameAttrPairs.slice(0, count);
}

function populateGCEventInfoTable(data) {
	var table = $("#gcEventInfoTable")[0];
	var youngGenGCInfo = data[0];
	data.forEach(function(rowData, i) {
		var row = table.rows[i + 1];
		rowData.forEach(function(v, i) {
			row.cells[i].innerHTML = v;
		});
	});
}

function startDebugSession() {
	if ((viewState.degFile != "") && (viewState.profileDataFile != "")) {
		setGlobalStatsMetric($("#globalStatsMetric").val())
		setUpSelectTagForDEGView()

		editor = createEditor("code")
  		profData = getProfileData(degOps, profileData.Profile, config)
  		graphController = createDataFlowGraph(cola, "#dfg", profData.dependencyData, viewState, config)
  		//graphController = {}

  		// This is the data to be visualized using bar charts
  		topNodesBasedOnTime = getTopNodesBasedOnTotalTime(profData.executionProfile.nodeNameToSummary, profData.dependencyData, 20);
  		topNodesBasedOnMemUsage = getTopNodesBasedOnMemUsage(profData.executionProfile.nodeNameToSummary, profData.dependencyData, 20);

  		maxTimeTakenByAKernel = topNodesBasedOnTime[0].totalTimePct;
  		maxMemUsageByAKernel = topNodesBasedOnMemUsage[0].memUsage;
  		setTimeAndMemColorScales();

		threadLevelSyncStats = profData.executionProfile.threadLevelPerfStats.map(function(o, i) {return {
  			name: "T" + i,
  			syncTimePct: o.syncTime.pct
  		}})

      	timelineController = new TimelineGraph("mainTimeline", "-main", "#timeline", profData, "#timelineHiddenNodeList", config)
      	timelineController.draw();
  			
  		$("#timelineHiddenNodeList").change({
  			graph: timelineController
  		}, timelineController.timelineScopeSelHandler) 

      	createStackGraph("#memory", profData.executionProfile.memUsageData, timelineController.xScale)
      	createGCStatsGraph("#gcStats", gcEvents, timelineController.xScale, config)

		setUpSynchronizedScrolling();
		//lockScrollingOfComparisonRuns();
    } else {
    	alert("Please upload the DEG file and the profile data (profData.js) and retry");
    }
}

function setUpSynchronizedScrolling() {
	$("#timelineWrapper-main").on("scroll", function() {
		var tmp = $("#timelineWrapper-main")[0];
		$("#memory")[0].scrollLeft = tmp.scrollLeft;
		$("#gcStats")[0].scrollLeft = tmp.scrollLeft;
	});
}

function lockScrollingOfComparisonRuns() {
	$("#viewRunsDiv").on("scroll", function() {
		var scrollAmt = $("viewRunsDiv")[0].scrollLeft;
		$(".comp-timeline").scrollLeft(scrollAmt);
	});
}

function searchNode(nodeName) {
	var nodeId = profData.dependencyData.nodeNameToId[nodeName]
	var node = profData.dependencyData.nodes[nodeId]
	config.populateKernelInfoTable(node)

	var sc = node.sourceContext
	config.highlightLineInEditor(sc.file, sc.line)

	graphController.markNeighbors(nodeId)
}