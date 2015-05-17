
var runSummariesChart = "#compareRunSummariesDiv";
var uploadRunSummariesBtnId = "#_uploadRunSummariesBtn";
var initializeViewsBtnId = "#initializeViewsBtn";

var xSeries = [];

var dataSeriesColls = {
	"TOTAL_TIME" : {},
	"MEM_USAGE"  : {}
};

var yAxisLabels = {
	"TOTAL_TIME" : "Time (ms)",
	"MEM_USAGE"	 : "Memory Allocated (B)"
};

$(uploadRunSummariesBtnId).on("change", readExecutionProfiles);
$(initializeViewsBtnId).on("click", initializeViews);
$("#compareRunSummariesMetricOptions").change(function() {
	initializeViews();
});

var kernelSummariesChart = "#compareKernelSummariesDiv";
$('#compareSummariesOfKernelTxtBx').keyup(function(event){
    if(event.keyCode == 13){
        displaySummariesOfKernel($(this).val());
    }
});

$("#compareKernelSummariesMetricOptions").change(function() {
	displaySummariesOfKernel("");
});

var threadCountToProfileDB = {};
var fileToProcessingDone = {};
var kernelSummariesDisplayed = {};

function enableViewDataBtnIfAllProcDone() {
	for (var file in fileToProcessingDone) {
		if (!(fileToProcessingDone[file])) {
			$("#initializeViewsBtn")[0].disabled = true;
			return;
		}
	}

	$("#initializeViewsBtn")[0].disabled = false;
}

function readExecutionProfiles(evt) {
	var files = evt.target.files;
	if (files.length > 0) {
		for (var i = 0; i < files.length; i++) {
			var reader = new FileReader();
			var file = files[i];

			reader.onload = (function(currFile) {
				var fileName = file.name;

				return function(e) {
					var Uints = new Uint8Array(e.target.result);
					var db = new ProfileDB( new SQL.Database(Uints) );
					var tc = db.threadCount();
					threadCountToProfileDB[tc] = db;
					fileToProcessingDone[fileName] = true;
					enableViewDataBtnIfAllProcDone();
			}})(file);

			fileToProcessingDone[file.name] = false;
			$("#initializeViewsBtn")[0].disabled = true;
			reader.readAsArrayBuffer( file );
		}
	}
}

function initializeViews( evt ) {
	var metric = $("#compareRunSummariesMetricOptions").val();
	var dataSeries = {};
	var numExecProfiles = 0;

	for ( var n in threadCountToProfileDB ) {
		numExecProfiles++;
		xSeries.push(n);
		var db = threadCountToProfileDB[n];
		var summaries = db.ticTocRegionSummaries();
		for (var i in summaries) {
			var s = summaries[i];
			addToMap( dataSeries, s.NAME, s.TOTAL_TIME );
		}
	}

	createLineChart( runSummariesChart, xSeries, dataSeries, "Number of Threads", "Time (ms)" );
}

function setUpSyncScrollingForTimelines(numTimelines) {
	var idSelectorForLastTimeline = toIdSelector(TIMELINE_CONTAINER_ID_PREFIX + "-" + numTimelines);
	$(idSelectorForLastTimeline).on("scroll", function() {
		var tmp = $(idSelectorForLastTimeline)[0];
		for (var i = 1; i < numTimelines; i++) {
			$("#" + TIMELINE_CONTAINER_ID_PREFIX + "-" + i)[0].scrollLeft = tmp.scrollLeft;
		}
	});
}

function createLineChart(parentDivIdSelector, xSeries, dataSeries, xAxisLabel, yAxisLabel) {
	var cols = [];
	var xCol = ['x'];

	for (var i in xSeries) {
		xCol.push(xSeries[i]);
	}

	cols.push(xCol);

	for (var key in dataSeries) {
		var col = [key];
		var vals = dataSeries[key];
		for (var i in vals) {
			col.push(vals[i]);
		}

		cols.push(col);
	}

	var chart = c3.generate({
	    bindto: parentDivIdSelector,
	    data: {
	    	x : 'x',
      		columns: cols
	    },
	    axis: {
	    	x: { label: xAxisLabel },
	    	y: { label: yAxisLabel }
	    }
	});
}

function displaySummariesOfKernel( kernel ) {
	function getOrElse(s, m) {
		var tmp = s[m];
		return (tmp != undefined) ? tmp : 0;
	}

	var metric = $("#compareKernelSummariesMetricOptions").val();
	var dataSeries = dataSeriesColls[metric];

	if (kernel != "") {
		if ( !(kernel in kernelSummariesDisplayed) ) {
			kernelSummariesDisplayed[kernel] = [];
		
			for ( var n in threadCountToProfileDB ) {
				var db = threadCountToProfileDB[n];
				var s = db.dbExecutionSummaryByName( kernel );
				addToMap( dataSeriesColls["TOTAL_TIME"], kernel, getOrElse( s, "TOTAL_TIME") );
				addToMap( dataSeriesColls["MEM_USAGE"], kernel, getOrElse( s, "MEM_USAGE") );
			}
		}
	}

	createLineChart( kernelSummariesChart, xSeries, dataSeries, "Number of Threads", yAxisLabels[metric] );
}

function displayTimelineView(executionProfile, xScale) {
	createHeaderDiv(divId, containerDivId);
	$("#" + getHeaderDivId(divId)).append(executionProfile.fileName);
	createCloseButton(divId, "#" + getHeaderDivId(divId));
	createRunDiv(divId, containerDivId);

	timelineDataModel = {
		"executionProfile": executionProfile,
		"dependencyData": profData.dependencyData,
	};

	var timelineClassStr = "comp-timeline";
	var timelineElemsNameSuffix = "-" + divId;
	var timelineParentDivId = "#" + getRunDivId(divId);
	var timelineLevelSelectionId = "#" + getLevelSelectorId(divId);
	var timeline = new TimelineGraph(timelineClassStr, timelineElemsNameSuffix, timelineParentDivId, 
									 timelineDataModel, timelineLevelSelectionId ,config);
	timeline.parentDivWidth = $("#tabs").width();
	timeline.parentDivHeight = $("#panel-3").height() * 0.3;
	timeline.xScale = xScale;
	timeline.jvmUpTimeAtAppStart = executionProfile.jvmUpTimeAtAppStart;
	timeline.draw();

	createLevelSelector(divId, "#" + getHeaderDivId(divId), timeline);

	divId++;
}

function getCommmonXScaleForTimelineComp(threadCountToExecutionProfile) {
	var maxTc = 0;
	var maxAppTime = Number.MIN_VALUE;

	for (var tc in threadCountToExecutionProfile) {
		var p = threadCountToExecutionProfile[tc];
		if (p.totalAppTime > maxAppTime) {
			maxTc = tc;
			maxAppTime = p.totalAppTime;
		}
	}

	var	timeBegin = 0 - (maxAppTime * 0.01);
	var	timeEnd = maxAppTime * 1.01;
	return d3.scale.linear()
		.domain([timeBegin, timeEnd + 50])
		.range([120, $("#tabs").width() * 2.5]);
}

function cloneXScale(xScale) {
	var clone = d3.scale.linear().domain(xScale.domain()).range(xScale.range());
	return clone;
}

function addToMap(dict, k, v) {
	if (!(k in dict)) {
		dict[k] = [];
	}

	dict[k].push(v);
}
