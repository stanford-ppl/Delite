
// =================================================
//	Global variables
// =================================================

containerDivId = "#viewRunsDiv";
addRunBtnId = "#_addProfileDataBtn";
divId = 1; // the id # of the next div that would be created to display a new profile dataset

// =================================================
//	Main functions
// =================================================

///*
$(addRunBtnId).on("change", appendRunView)

function appendRunView(evt) {
	var reader = new FileReader();
	var dataFileName = ""
	reader.onload = function(e) {
    	createHeaderDiv(divId, containerDivId);
		$("#" + getHeaderDivId(divId)).append(dataFileName);
		createCloseButton(divId, "#" + getHeaderDivId(divId));
		createRunDiv(divId, containerDivId);

		data = JSON.parse(e.target.result);
		timelineDataModel = {
			"executionProfile": getExecutionProfile(data.Profile, profData.dependencyData, config),
			"dependencyData": profData.dependencyData,
		};

		var timelineClassStr = "comp-timeline";
		var timelineElemsNameSuffix = "-" + divId;
		var timelineParentDivId = "#" + getRunDivId(divId);
		var timelineLevelSelectionId = "#" + getLevelSelectorId(divId);
		var timeline = new TimelineGraph(timelineClassStr, timelineElemsNameSuffix, timelineParentDivId, 
										 timelineDataModel, timelineLevelSelectionId ,config);
		timeline.draw();

		createLevelSelector(divId, "#" + getHeaderDivId(divId), timeline);

		divId++;
    };

	if (evt.target.files.length > 0) {
		file = evt.target.files[0]
		dataFileName = file.name
		reader.readAsText(file);
		this.value = null; // resetting the value in order to enable the "onchange" event even if the user selects another file with the same name
	}
}
//*/

function closeRunView(i) {
	$("#" + getHeaderDivId(i)).remove();
	$("#" + getCloseButtonId(i)).remove();
	$("#" + getRunDivId(i)).remove();
}

// =================================================
//  Helper functions
// =================================================

function createHeaderDiv(i, parentDivId) {
	$("<div/>", {
	   id: getHeaderDivId(i),
	   "class": 'run-headers',
	}).appendTo(parentDivId);
}

function createCloseButton(i, parentDivId) {
	$("<button/>", {
		id 		: getCloseButtonId(i),
		"class" : "close-run",
		text	: "close",
		onclick	: "closeRunView(" + i + ")"
	}).appendTo(parentDivId)
}

function createRunDiv(i, parentDivId) {
	$("<div/>", {
	   id: getRunDivId(i),
	   "class": 'runs runs-' + (i % 2)
	}).appendTo(parentDivId);
}

function createLevelSelector(i, parentDivId, timeline) {
	$("<select/>", {
		id 		: getLevelSelectorId(i),
		"class" : "timeline-level-selector",
		text	: "Level 0"
	}).appendTo(parentDivId);

	$("#" + getLevelSelectorId(i)).change({
		graph: timeline
	}, timeline.timelineScopeSelHandler) 
}

function getHeaderDivId(i) {
	return 'run-header-' + i;
}

function getCloseButtonId(i) {
	return "close-run-" + i;
}

function getRunDivId(i) {
	return 'run-' + i;
}

function getLevelSelectorId(i) {
	return "timeline-level-selector-" + i;
}

// =================================================

/*
$(addRunBtnId).on("change", addRunViews)

function addRunViews(evt) {
	var files = evt.target.files;
	for (var i = 0; i < files.length; i++) {
		var file = files[i];
		readFile(file);
	}
}

function readFile(file) {
	var reader = new FileReader();
	var fileName = file.name;
	reader.onload = function(e) {
    	createHeaderDiv(divId, containerDivId);
		$("#" + getHeaderDivId(divId)).append(fileName);
		createCloseButton(divId, "#" + getHeaderDivId(divId));
		createRunDiv(divId, containerDivId);

		data = JSON.parse(e.target.result);
		timelineDataModel = {
			"executionProfile": getExecutionProfile(data.Profile, profData.dependencyData, config),
			"dependencyData": profData.dependencyData,
		};

		var timelineClassStr = "comp-timeline";
		var timelineElemsNameSuffix = "-" + divId;
		var timelineParentDivId = "#" + getRunDivId(divId);
		var timelineLevelSelectionId = "#" + getLevelSelectorId(divId);
		var timeline = new TimelineGraph(timelineClassStr, timelineElemsNameSuffix, timelineParentDivId, 
										 timelineDataModel, timelineLevelSelectionId ,config);

		timeline.draw();

		createLevelSelector(divId, "#" + getHeaderDivId(divId), timeline);

		divId++;
    };

    reader.readAsText(file);
}
//*/

