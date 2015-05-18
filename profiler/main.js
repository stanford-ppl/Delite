
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
config.highlightDNodeById = highlightDNodeById;
config.highlightLineInEditor = highlightLineInEditor;
config.highlightLineInEditorForDNode = highlightLineInEditorForDNode;

config.populateGCEventInfoTable = populateGCEventInfoTable;
config.populateKernelInfoTable = populateKernelInfoTable;
config.populateKernelInfoTableById = populateKernelInfoTableById;
config.populateSyncNodeInfoTable = populateSyncNodeInfoTable;

config.profileDB = undefined;

config.dNodeInfoTable = $("#kernelInfoTable")[0];
config.enableNodeClickHandler = true; // for bar-charts.
config.TNODE_TYPE_SYNC = 2;

//config.syncNodeRegex = /__sync-ExecutionThread-(\d+)-(.*)-(.*)-(\d+)$/;
config.syncNodeRegex = /__sync-ExecutionThread(\d+)-(.*)-(.*)-(\d+)$/;
config.re_partition = /^(.*)_(\d+)$/;
config.re_header = /^(.*)_h$/;

// NOTE: This must be kept in sync with the order in which the DNodeType enums are defined in the PostProcessor scalacode
config.dNodeTypeIdToType = {
	0 : 'WhileLoop',
	1 : 'Conditional',
	2 : 'MultiLoop',
	3 : 'SingleTask',
	4 : 'Foreach',
	5 : 'EOG',
	6 : 'EOP'
};

addAppSourceFileHandler("srcDirInput", editor);
addDegFileHandler("degFileInput");
addProfileDataFileHandler("profDataInput");
addGCStatsFileHandler("gcStatsInput");

var editor = {};
//var profData = {};
var graphController = {};
var timelineController = {};
var dbStmt = undefined;

config.MAX_NUM_TOP_NODES = 20;

// Input data sets for bar charts
var topNodesBasedOnL2CacheMissPct = undefined;
var topNodesBasedOnL3CacheMissPct = undefined;
var topNodesBasedOnMemUsage = undefined;
var topNodesBasedOnTime = undefined;
var threadLevelSyncStats = [];

function getTopDNodesBasedOnGivenMetric( outputArr, metric ) {
	if ( outputArr == undefined ) {
		var res = config.profileDB.fetchMultipleElemsFromDB( "SELECT * FROM DNodes INNER JOIN ExecutionSummaries ON DNodes.NAME == ExecutionSummaries.NAME ORDER BY " + metric + " DESC" );
		outputArr = res.slice( 0, config.MAX_NUM_TOP_NODES );
	}

	return outputArr;
}

function getTopDNodesBasedOnMemMetric( outputArr, metric ) {
	if ( outputArr == undefined ) {
		var res = config.profileDB.fetchMultipleElemsFromDB( "SELECT * FROM KernelMemAccessStats ORDER BY " + metric + " DESC" );
		outputArr = res.slice( 0, config.MAX_NUM_TOP_NODES );
	}

	return outputArr;
}

$("#globalStatsMetric").change(function() {
	function helper( d ) {
		var arr = d.NAME.split(":");
		config.highlightLineInEditor( arr[0], parseInt(arr[1]) );
	}

	$("#generalInfo").hide();
	$("#dfgHeader").show();
	$("#dfg").show();

	var metric = $(this).val();
	if (metric == "performance") {
		clearDivForBarChartDisplay();
		var res = getTopDNodesBasedOnGivenMetric( topNodesBasedOnTime, "TOTAL_TIME" );
		createBarChart("#dfg", res , "TOTAL_TIME", getDisplayTextForTime, config);
	} else if (metric == "memUsage") {
		clearDivForBarChartDisplay();
		var res = getTopDNodesBasedOnGivenMetric( topNodesBasedOnMemUsage, "MEM_USAGE" );
		createBarChart("#dfg", res, "MEM_USAGE", getDisplayTextForMemUsage, config);
	} else if (metric == "l2CacheMissRatio") {
		clearDivForBarChartDisplay();
		var res = getTopDNodesBasedOnMemMetric( topNodesBasedOnL2CacheMissPct, "L2_CACHE_MISS_PCT" );
		createBarChart("#dfg", res, "L2_CACHE_MISS_PCT", getDisplayTextForL2CacheMissRatio, config, helper);
	} else if (metric == "l3CacheMissRatio") {
		clearDivForBarChartDisplay();
		var res = getTopDNodesBasedOnMemMetric( topNodesBasedOnL3CacheMissPct, "L3_CACHE_MISS_PCT" );
		createBarChart("#dfg", res, "L3_CACHE_MISS_PCT", getDisplayTextForL3CacheMissRatio, config, helper);
	} else if (metric == "threadLevelSyncStats") {
		clearDivForBarChartDisplay();
		createBarChart("#dfg", threadLevelSyncStats, "syncTimePct", getDisplayTextForThreadLevelSync, config);
	//} else if (metric == "ticTocRegionStats") {
	//	displayOverallRegionsData();
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
        searchDNode( $(this).val() );
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
	return d.NAME + " (" + getDisplayTextForTimeAbsPctPair( d.TOTAL_TIME, d.TOTAL_TIME_PCT ) + ")";
}

function getDisplayTextForMemUsage(d) {
	return d.NAME + " (" + memUsageValueToStr( d.MEM_USAGE ) + ")";
}

function getDisplayTextForL2CacheMissRatio(d) {
	return d.NAME + " (" + d.L2_CACHE_MISS_PCT + "%)";
}

function getDisplayTextForL3CacheMissRatio(d) {
	return d.NAME + " (" + d.L3_CACHE_MISS_PCT + "%)";
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

function highlightDNodeById(dNodeId) {
	var n = viewState.highlightedGraphNode;
	if (n != -1) { graphController.unhighlightNode(n); }

	graphController.highlightNode( dNodeId );
	graphController.highlightDNodeById( dNodeId );
	viewState.highlightedGraphNode = dNodeId;
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

function highlightLineInEditorForDNode( dNode ) {
	var sc = dNode.SOURCE_CONTEXT;
	var arr = sc.split(":");
	highlightLineInEditor( arr[0], parseInt(arr[1]) );
}

/*
function highlightLineInEditorByKernelId(nodeId) {
	var node = profData.dependencyData.nodes[nodeId]
	var sc = node.sourceContext
	highlightLineInEditor(sc.file, sc.line)
}
*/

function populateKernelInfoTable( dNode ) {
	var name = dNode.NAME;
	//var summary = dbExecutionSummaryByName(name);
	var summary = config.profileDB.dbExecutionSummaryByName(name);
	var isSummaryPresent = (summary.TOTAL_TIME != undefined);
	
	var type = config.dNodeTypeIdToType[dNode.TYPE];

	var appTotalTime = postProcessedProfile.AppData.appTotalTime;
	var totalTimeAbs = isSummaryPresent ? summary.TOTAL_TIME : 0;
	var totalTimePct = ((totalTimeAbs * 100) / appTotalTime);
	var timeStr = getDisplayTextForTimeAbsPctPair( totalTimeAbs, totalTimePct );

	var execTime = isSummaryPresent ? summary.EXEC_TIME : 0;
	var execTimePct = ((execTime * 100) / totalTimeAbs);
	var syncTimePct = 100 - execTimePct;
	var execSyncTimeStr = execTimePct + "/" + syncTimePct + " %";

	var memUsage = isSummaryPresent ? summary.MEM_USAGE : 0;

	var values = [name, type, timeStr, execSyncTimeStr, memUsage];

	values.forEach(function(v, i) {
		var row = config.dNodeInfoTable.rows[i + 1];
		row.cells[1].innerHTML = values[i];
	});
}

function populateKernelInfoTableById(dNodeId) {
	//var dNode = dbDNodeById( dNodeId );
	var dNode = config.profileDB.dbDNodeById( dNodeId );
	populateKernelInfoTable( dNode );
	return dNode;
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

	nodeNameAttrPairs.sort(function(p1, p2) { return p2.memUsage - p1.memUsage; });

	return nodeNameAttrPairs.slice(0, count);
}

function getNodesWithHighestL2AndL3MissRatios(nodeNameToMemAccessStats, count) {
	var nodeL2MissPctPairs = [];
	var nodeL3MissPctPairs = [];
	for (var name in nodeNameToMemAccessStats) {
		var sumOfL2HitPcts = 0;
		var sumOfL3HitPcts = 0;
		var count = 0;
		var instanceToStats = nodeNameToMemAccessStats[name];
		for (var i in instanceToStats) {
			var tidToStats = instanceToStats[i];
			for (var tid in tidToStats) {
				var stats = tidToStats[tid];
				sumOfL2HitPcts += 100 - stats.l2CacheHitPct;
				sumOfL3HitPcts += 100 - stats.l3CacheHitPct;
				count += 1;
			}
		}

		nodeL2MissPctPairs.push({
			"name": name,
			"missPct": 100 - Math.round(sumOfL2HitPcts / count)
		});

		nodeL3MissPctPairs.push({
			"name": name,
			"missPct": 100 - Math.round(sumOfL3HitPcts / count)
		});
	}

	nodeL2MissPctPairs.sort(function(p1, p2) { return p2.missPct - p1.missPct });
	nodeL3MissPctPairs.sort(function(p1, p2) { return p2.missPct - p1.missPct });

	return [nodeL2MissPctPairs.slice(0, count), nodeL3MissPctPairs.slice(0, count)];
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

  		var dependencyData = postProcessedProfile.DependencyGraph;
  		dependencyData.nodes.pop();
  		dependencyData.edges.pop();
  		graphController = createDataFlowGraph(cola, "#dfg", dependencyData.nodes, dependencyData.edges, viewState, config);

  		/*
  		maxTimeTakenByAKernel = topNodesBasedOnTime[0].totalTimePct;
  		maxMemUsageByAKernel = topNodesBasedOnMemUsage[0].memUsage;
  		setTimeAndMemColorScales();

		threadLevelSyncStats = profData.executionProfile.threadLevelPerfStats.map(function(o, i) {return {
  			name: "T" + i,
  			syncTimePct: o.syncTime.pct
  		}})
		*/

      	timelineController = new TimelineGraph("mainTimeline", "-main", "#timeline", postProcessedProfile.AppData, postProcessedProfile.TimelineData, "#timelineHiddenNodeList", config)

      	timelineController.draw();

  		
  		$("#timelineHiddenNodeList").change({
  			graph: timelineController
  		}, timelineController.timelineScopeSelHandler) 
		
      	createStackGraph("#memory", postProcessedProfile.MemUsageSamples, timelineController.xScale);
      	createGCStatsGraph("#gcStats", gcEvents, timelineController.xScale, config);

      	
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

function searchDNode( dNodeName ) {
	var dNode = dbDNodeByName( dNodeName );
	graphController.highlightDNodeById( dNode.ID ); // TODO: This is inefficient. The higlightDNodeById() function again fetches the dNode from the database.
}