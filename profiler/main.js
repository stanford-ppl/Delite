
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

config.syncNodeRegex = /__sync-ExecutionThread(\d+)-(.*)-(.*)-(\d+)$/;
config.re_partition = /^(.*)_(\d+)$/;
config.re_header = /^(.*)_h$/;

// HACK: Temporary solution until source file reading has been ported to server-based implementation
var fileNameToFile = {}

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

var editor = {};
var graphController = {};
var timelineController = {};
var dbStmt = undefined;
var barChartController = new BarChartController( "#dfg", config );

config.MAX_NUM_TOP_NODES = 10;

$("#globalStatsMetric").change(function() {
	function helper( d ) {
		var arr = d.NAME.split(":");
		config.highlightLineInEditor( arr[0], parseInt(arr[1]) );
	}

	$("#generalInfo").hide();
	$("#dfgHeader").show();
	$("#dfg").show();

	var metric = $(this).val();
	if (metric == "degView") {
		$(".barChart").hide();
		$("#dfg").css("overflow-y", "hidden");
		$('.dataflowSvg').show();
		setUpSelectTagForDEGView();
		graphController.changeColoringScheme("dataDeps");
	} else {
		clearDivForBarChartDisplay();
		barChartController.display( metric );
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

$('#startButton').click(startDebugSession);

$(window).resize(function() {
	$("#right").css("width", $("#container").width() - $("#accordian").width());
});

$(document).ready(function() {
	$("#right").css("width", $("#container").width() - $("#accordian").width());
	$("#dfg").css("height", $("#dfg").height() - $("#dfgHeader").height());
});

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

function highlightDNodeById(dNodeId) {
	var n = viewState.highlightedGraphNode;
	if (n != -1) { graphController.unhighlightNode(n); }

	graphController.highlightNode( dNodeId );
	graphController.highlightDNodeById( dNodeId );
	viewState.highlightedGraphNode = dNodeId;
}

function highlightLineInEditor( fileName, line ) {
	function helper() {
		viewState.highlightedLine = highlightLine( line );
		viewState.appSourceFileName = fileName;
		$( "#srcFileName" ).text( fileName );
	}

	function onFileLoad( contents ) {
		editor.setValue( contents, -1 );
		helper();
	}

	unhighlightLine(viewState.highlightedLine);
	if ( fileName != viewState.appSourceFileName ) {
		var url = hostURL + '?f=appSourceFile&name=' + fileName; 
		sendReq( url, onFileLoad );
	} else {
		helper();
	}
}

function highlightLineInEditorForDNode( dNode ) {
	var sc = dNode.SOURCE_CONTEXT;
	var arr = sc.split(":");
	highlightLineInEditor( arr[0], parseInt(arr[1]) );
}

function populateKernelInfoTable( dNode ) {
	function f( summary ) {
		var isSummaryPresent = (summary != undefined);
	
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
		var memUsageStr = memUsageValueToStr( memUsage );

		var values = [name, type, timeStr, execSyncTimeStr, memUsageStr];

		values.forEach(function(v, i) {
			var row = config.dNodeInfoTable.rows[i + 1];
			row.cells[1].innerHTML = values[i];
		});
	}

	var name = dNode.NAME;
	config.profileDB.dbExecutionSummaryByName( name, f );
}

function populateKernelInfoTableById(dNodeId) {
	function f( dNode ) {
		populateKernelInfoTable( dNode );
	}

	config.profileDB.dbDNodeById( dNodeId, f );
}

function populateSyncNodeInfoTable(node) {
	var properties = ["Dep. Thread", "Dep. Kernel", "Time (%)"];
	var m = node.name.match(config.syncNodeRegex);
	var values = ["T" + m[4], m[3], "NA"];
	var table = $("#syncNodeInfoTable")[0];
	properties.forEach(function(p, i) {
		var row = table.rows[i + 1];
		row.cells[1].innerHTML = values[i];
	});
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

function isInitialDataFetched() {
	return (isUIDataFetched && isGCStatsFetched);
}

function startDebugSession() {
	setGlobalStatsMetric($("#globalStatsMetric").val())
	setUpSelectTagForDEGView()

	editor = createEditor("code")

	var dependencyData = postProcessedProfile.DependencyGraph;
	dependencyData.nodes.pop();
	dependencyData.edges.pop();
	graphController = createDataFlowGraph(cola, "#dfg", dependencyData.nodes, dependencyData.edges, viewState, config);

  	//maxTimeTakenByAKernel = topNodesBasedOnTime[0].totalTimePct;
	//maxMemUsageByAKernel = topNodesBasedOnMemUsage[0].memUsage;
  	//setTimeAndMemColorScales();

	//threadLevelSyncStats = profData.executionProfile.threadLevelPerfStats.map(function(o, i) {return {
  	//	name: "T" + i,
  	//	syncTimePct: o.syncTime.pct
  	//}})

  	timelineController = new TimelineGraph("mainTimeline", "-main", "#timeline", postProcessedProfile.AppData, postProcessedProfile.TimelineData, "#timelineHiddenNodeList", config)

  	timelineController.draw();

		
		$("#timelineHiddenNodeList").change({
			graph: timelineController
		}, timelineController.timelineScopeSelHandler) 
	
  	createStackGraph("#memory", postProcessedProfile.MemUsageSamples, timelineController.xScale);
  	createGCStatsGraph("#gcStats", gcEvents, timelineController.xScale, config);

	setUpSynchronizedScrolling();
	//lockScrollingOfComparisonRuns();
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
	function f( dNode ) {
		graphController.highlightDNodeById( dNode.ID ); // TODO: This is inefficient. The higlightDNodeById() function again fetches the dNode from the database.
	}
	
	config.profileDB.dbDNodeByName( dNodeName, f );
}

// =====================================================
// Contact server and get uiData and gcStats
// =====================================================

var hostURL = 'http://localhost:8000';
var isUIDataFetched = false;
var isGCStatsFetched = false;
var isErrorInFetch = false;
var server = new Server( hostURL );
config.profileDB = new RemoteProfileDB( server );

function parseGCStats( response ) { 
  var t = parseInt(postProcessedProfile.AppData.jvmUpTimeAtAppStart);
  gcEvents = parseGCStatsDump(response, t);
  isGCStatsFetched = true;
  startDebugSession();
}

function parseUIData( response ) { 
  postProcessedProfile = JSON.parse( response ).Profile;
  isUIDataFetched = true;
  sendReq( hostURL + '?f=gcStats', parseGCStats );
}

function parseErr( response ) {
  err = response;
  isErrorInFetch = true;
}

function sendReq( url, parseResponse ) {
  var req = new XMLHttpRequest();
  req.open( 'GET', url, false );
  req.onreadystatechange = function() { parseResponse( req.responseText ) };
  req.send();
}

function fetchInitialDataFromServer() {
  sendReq( hostURL + '?f=profileDataUI', parseUIData );
}

window.onload = function() {
	fetchInitialDataFromServer();
}
