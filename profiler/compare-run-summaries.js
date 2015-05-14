
var runSummariesChart = "#compareRunSummariesDiv";
var uploadRunSummariesBtnId = "#_uploadRunSummariesBtn";
var initializeViewsBtnId = "#initializeViewsBtn";

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
	displaySummariesOfKernel($(this).val());
});

//var threadCountToExecutionProfile = {};
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

/*
function readExecutionProfiles(evt) {
	threadCountToExecutionProfile = {};
	var files = evt.target.files;
	if (files.length > 0) {
		for (var i = 0; i < files.length; i++) {
			var reader = new FileReader();
			var file = files[i];
			reader.onload = (function(currFile) {
				var fileName = file.name;

				return function(e) {
					var data = JSON.parse(e.target.result);
					var executionProfile = getExecutionProfile(data.Profile, profData.dependencyData, config);
					executionProfile.fileName = e.name;
					var numThreads = executionProfile.numThreads;
					if (!(numThreads in threadCountToExecutionProfile)) {
						threadCountToExecutionProfile[numThreads] = executionProfile;
						fileToProcessingDone[fileName] = true;
						enableViewDataBtnIfAllProcDone();
				}
			}})(file);

			fileToProcessingDone[file.name] = false;
			$("#initializeViewsBtn")[0].disabled = true;
			reader.readAsText(file);
		}
	}
}

function initializeViews(evt) {
	var metric = $("#compareRunSummariesMetricOptions").val();
	var xSeries = [];
	var dataSeries = {};
	var xScale = getCommmonXScaleForTimelineComp(threadCountToExecutionProfile);
	var numExecProfiles = 0;

	for (var n in threadCountToExecutionProfile) {
		numExecProfiles++;
		xSeries.push(n);
		executionProfile = threadCountToExecutionProfile[n];
		for (var i in executionProfile.ticTocRegions) {
			var region = executionProfile.ticTocRegions[i];
			var absTime = 0;
			if (metric == "totalTime") {
				absTime = region.totalTime.abs;
			} else if (metric == "execTime") {
				absTime = region.execTimeStats[0].abs; //TODO: Instead of always displaying stats for thread 0, we need to have a thread-selection option
			} else if (metric == "syncTime") {
				absTime = region.syncTimeStats[0].abs;
			}

			addToMap(dataSeries, region.name, absTime);
		}

		displayTimelineView(executionProfile, cloneXScale(xScale));
	}

	var idSelectorForLastTimeline = toIdSelector(TIMELINE_CONTAINER_ID_PREFIX + "-" + numExecProfiles);
	$(idSelectorForLastTimeline).css("overflow-x", "auto");
	setUpSyncScrollingForTimelines(numExecProfiles);

	createLineChart(runSummariesChart, xSeries, dataSeries, "Number of Threads", "Time (ms)");
	displaySummariesOfKernel("");
}
*/

function readExecutionProfiles(evt) {
	var files = evt.target.files;
	if (files.length > 0) {
		for (var i = 0; i < files.length; i++) {
			var reader = new FileReader();
			var file = files[i];

			reader.onload = (function(currFile) {
				var fileName = file.name;

				return function(e) {
					//var Uints = new Uint8Array(reader.result);
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
	var xSeries = [];
	var dataSeries = {};
	//var xScale = getCommmonXScaleForTimelineComp( threadCountToProfileDB );
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

		//displayTimelineView(executionProfile, cloneXScale(xScale));
	}

	//var idSelectorForLastTimeline = toIdSelector(TIMELINE_CONTAINER_ID_PREFIX + "-" + numExecProfiles);
	//$(idSelectorForLastTimeline).css("overflow-x", "auto");
	//setUpSyncScrollingForTimelines(numExecProfiles);

	createLineChart( runSummariesChart, xSeries, dataSeries, "Number of Threads", "Time (ms)" );
	//displaySummariesOfKernel("");
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
	var metric = $("#compareKernelSummariesMetricOptions").val();
	var xSeries = [];
	var dataSeries = {};

	if ( !(kernel in kernelSummariesDisplayed) ) {
		kernelSummariesDisplayed[kernel] = [];
	}

	for ( var n in threadCountToProfileDB ) {
		xSeries.push(n);
		var db = threadCountToProfileDB[n];
		for ( var k in kernelSummariesDisplayed ) {
			var s = db.dbExecutionSummaryByName(k);
			var absTime = ( s.TOTAL_TIME != undefined ) ? s.TOTAL_TIME : 0;
			addToMap( dataSeries, k, absTime );
		}
	}

	createLineChart( kernelSummariesChart, xSeries, dataSeries, "Number of Threads", "Time (ms)" );
}

/*
function displaySummariesOfKernel(kernel) {
	var metric = $("#compareKernelSummariesMetricOptions").val();
	var xSeries = [];
	var dataSeries = {};

	if (!(kernel in kernelSummariesDisplayed)) {
		kernelSummariesDisplayed[kernel] = [];
	}

	for (var n in threadCountToExecutionProfile) {
		xSeries.push(n);
		executionProfile = threadCountToExecutionProfile[n];
		for (var k in kernelSummariesDisplayed) {
			var summary = executionProfile.nodeNameToSummary[k];
			if (summary) {
				var absTime = 0;
				if (metric == "totalTime") {
					absTime = summary.totalTime.abs;
				} else if (metric == "execTime") {
					absTime = summary.execTime.abs;
				} else if (metric == "syncTime") {
					absTime = summary.syncTime.abs;
				}

				addToMap(dataSeries, k, absTime);
			}
		}
	}

	createLineChart(kernelSummariesChart, xSeries, dataSeries, "Number of Threads", "Time (ms)");
}
*/

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
