
// =========================================================
//  Global variables
// =========================================================
// Note the IDs defined here are not HTML IDs
TIMELINE_CONTAINER_ID_PREFIX = "timelineWrapper";
TIMELINE_SVG_ID_PREFIX = "chart";
TIMELINE_OUTERMOST_SVG_GROUP_PREFIX = "mini";
TIMELINE_LANELINE_CLASS_PREFIX = "laneLine";
TIMELINE_LANETEXT_CLASS_PREFIX = "laneText";
TIMELINE_NODE_CLASS_PREFIX = "node";
TIMELINE_NODE_LABEL_CLASS_PREFIX = "timelineNodeName";
TIMELINE_CHILDNODE_CLASS_PREFIX = "childNode"; // Child nodes are the ones created when a top-level timeline node is double-clicked
TIMELINE_CHILDNODE_LABEL_CLASS_PREFIX = "childNodeLabel";

LANE_COLORS = ["orange", "green", "lightskyblue", "red", "brown"];

// 'classStr'   => This can be used to group select a subset of all the timeline views being displayed
// 'nameSuffix'  => The name/id suffix to be used for all internally defined HTML classes/IDs
// 'parentDivId' => The HTML id of the div that would hold the timeline graph
// 'profileData' => The data model returned by the getProfileData() method defined in datamodel.js
// 'config'      => Holds the confguration state of the debug session. Also, holds pointers to the methods exposed by the other views

// TODO: The passing of 'timelineLevelSelectorId' as an input parameter is a hack. Instead, the TimelineGraph element
// should trigger an event when a node is double-clicked and external elements should be able to listen to it.
function TimelineGraph(classStr, nameSuffix, parentDivId, profileData, timelineLevelSelectorId, config) {
	this.classStr = classStr;
	this.nameSuffix = nameSuffix;
	this.parentDivId = parentDivId;
	this.executionProfile = profileData.executionProfile;
	this.timelineData = profileData.executionProfile.timelineData;
	this.dependencyData = profileData.dependencyData;
	this.timelineLevelSelectorId = timelineLevelSelectorId;
	this.config = config;
	this.laneColors = null;
	this.timelineGraph = null;
	this.chartWidth = 0;
	this.parentDivWidth = -1; // HACK: This field is used as a hack
	this.parentDivHeight = -1; // HACK: This field is used as a hack
	this.jvmUpTimeAtAppStart = 0; // HACK: This field is used as a hack
	this.xScale = null;
	this.yScale = null;
	this.initialXRange = null;
	this.stackOfHiddenNodes = Array();

	// HTML Class and Id names for different UI components
	this.containerId = TIMELINE_CONTAINER_ID_PREFIX + this.nameSuffix;
	this.svgId = TIMELINE_SVG_ID_PREFIX + this.nameSuffix;
	this.outermostSvgGroupId = TIMELINE_OUTERMOST_SVG_GROUP_PREFIX + this.nameSuffix;
	this.laneLineClass = TIMELINE_LANELINE_CLASS_PREFIX + this.nameSuffix;
	this.laneTextClass = TIMELINE_LANETEXT_CLASS_PREFIX + this.nameSuffix;
	this.timelineNodeClass = TIMELINE_NODE_CLASS_PREFIX + this.nameSuffix;
	this.timelineNodeLabelClass = TIMELINE_NODE_LABEL_CLASS_PREFIX + this.nameSuffix;
	this.timelineChildNodeClass = TIMELINE_CHILDNODE_CLASS_PREFIX + this.nameSuffix;
	this.timelineChildNodeLabelClass = TIMELINE_CHILDNODE_LABEL_CLASS_PREFIX + this.nameSuffix;
}

TimelineGraph.prototype.rectHeight = 20;
TimelineGraph.prototype.re_partition = /^(.*)_(\d+)$/;
TimelineGraph.prototype.re_header = /^(.*)_h$/;

TimelineGraph.prototype.draw = function() {
	var items = this.convertDataToTimelineFormat(this.timelineData.timing); 
	var lanes = this.timelineData.lanes;
	this.laneColors = lanes.map(function(l, i) {return LANE_COLORS[i % LANE_COLORS.length]});

	var tmp = this.getAppBeginAndEndTimes();
	var	timeBegin = tmp.begin - tmp.duration * 0.01;
	var	timeEnd = tmp.end + tmp.duration * 0.01;

	var parentDiv = $(this.parentDivId);
	if (this.parentDivWidth == -1) {
		this.parentDivWidth = parentDiv.width();
	}

	if (this.parentDivHeight == -1) {
		this.parentDivHeight = parentDiv.height();
	}

	var	chartWidth = this.parentDivWidth * 2.5;
	var	chartHeight = this.parentDivHeight;

	this.chartWidth = chartWidth;

	var m = [20, 15, 15, 120]; //top right bottom left
    var initialXRange = [m[3], chartWidth]
	var x = d3.scale.linear()
		.domain([timeBegin, timeEnd + 50])
		.range(initialXRange);

	this.initialXRange = initialXRange;
	if (this.xScale == null) {
		this.xScale = x;
	} else if (this.jvmUpTimeAtAppStart > 0) {
		var currXDomain = this.xScale.domain();
		this.xScale.domain([currXDomain[0] + this.jvmUpTimeAtAppStart, currXDomain[1] + this.jvmUpTimeAtAppStart]);	
	}

	var	numLanes = lanes.length;
	var y = d3.scale.linear()
		.domain([0, numLanes])
		.range([0, chartHeight]);
	this.yScale = y;
	
	var div2 = d3.select(this.parentDivId)
		.append("div")
		.attr("float", "right")
		.attr("class", TIMELINE_CONTAINER_ID_PREFIX + " " + this.classStr)
		.attr("id", this.containerId);

	$(toIdSelector(this.containerId)).css("width", "" + this.parentDivWidth + "px")

	var chart = div2
		.append("svg")
		.attr("width", chartWidth)
		.attr("height", chartHeight)
		.attr("class", TIMELINE_SVG_ID_PREFIX)
		.attr("id", this.svgId);

	this.timelineGraph = chart.append("g")
		.attr("width", chartWidth)
		.attr("class", TIMELINE_OUTERMOST_SVG_GROUP_PREFIX)
		.attr("id", this.outermostSvgGroupId);

	this.timelineGraph.append("g").selectAll(toClassSelector(this.laneLineClass))
		.data(lanes)
		.enter().append("line")
		.attr("x1", m[3])
		.attr("y1", function(d, i) {return y(i + .5);})
		.attr("x2", chartWidth)
		.attr("y2", function(d, i) {return y(i + .5);})
		.attr("stroke", "black")
		.attr("class", this.laneLineClass)
		.style("opacity", "0.2")

	this.timelineGraph.append("g").selectAll(toClassSelector(this.laneTextClass))
		.data(lanes)
		.enter().append("text")
		.text(function(d) {return d;})
		.attr("x", m[3] - 60)
		.attr("y", function(d, i) {return y(i + .5);})
		.attr("dy", ".5ex")
		.attr("text-anchor", "end")
		.attr("class", this.laneTextClass);

	this.createTimelineNodes(items, this.timelineNodeClass);
	this.createTicTocRegionNodes(this.executionProfile.ticTocRegions, this.timelineNodeClass);

	//timeline labels
	var minDurationReqForDisplayingLabel = 0.05 * this.executionProfile.totalAppTime;
	var eventsWithLabel = items.filter(function(d) {return (d.end - d.start) >= minDurationReqForDisplayingLabel});
	this.createTimelineLabels(eventsWithLabel, this.timelineNodeLabelClass);
	this.createTicTocRegionLabels(this.executionProfile.ticTocRegions, this.timelineNodeLabelClass);
};

TimelineGraph.prototype.convertDataToTimelineFormat = function(data) {
	var res = [];
	var runs = data[0];
	for (node in runs) {
		res = res.concat(runs[node]);
	}

	return res;
};

TimelineGraph.prototype.getAppBeginAndEndTimes = function() {
	return {"begin": this.executionProfile.appStartTime, 
			"end": this.executionProfile.appEndTime, 
			"duration": this.executionProfile.totalAppTime};
};

TimelineGraph.prototype.createTimelineNodes = function(data, className) {
	var graph = this;
	var x = graph.xScale;
	var y = graph.yScale;

	this.timelineGraph.append("g").selectAll("." + className)
		.data(data)
		.enter().append("rect")
		.attr("class", function(d) {return className + " " + graph.getClassNameForRect(d)})
		.attr("x", function(d) {return x(d.start);})
		.attr("y", function(d) {return y(d.lane + .5) - graph.rectHeight/2;})
		.attr("width", function(d) {return x(d.end) - x(d.start);})
		.attr("height", graph.rectHeight)
		.attr("id", function(d) {return "" + d.id + graph.nameSuffix})
		.attr("name", function(d) {return graph.getNodeName(d.name)})
		.attr("vector-effect", "non-scaling-stroke") // from http://stackoverflow.com/questions/10357292/how-to-make-stroke-width-immune-to-the-current-transformation-matrix
		.style("fill", function(d) {return graph.getRectFill(d)})
		.on("click", function(d) {return graph.timelineNodeClickHandler(d)})
		.on("dblclick", function(d) {return graph.dblClickHandler(d)});
};

TimelineGraph.prototype.createTicTocRegionNodes = function(ticTocRegions, className) {
	var graph = this;
	var x = graph.xScale;
	var y = graph.yScale;
	var numThreads = this.executionProfile.numThreads;

	this.timelineGraph.append("g").selectAll("." + className)
		.data(ticTocRegions)
		.enter().append("rect")
		.attr("class", function(d) {return className + " " + graph.getClassNameForRect(d)})
		.attr("x", function(d) {return x(d.start);})
		.attr("y", function(d) {return y(numThreads + .5) - graph.rectHeight/2;})
		.attr("width", function(d) {return x(d.end) - x(d.start);})
		.attr("height", graph.rectHeight)
		.attr("id", function(d) {return "" + d.id + graph.nameSuffix})
		.attr("name", function(d) {return d.name})
		.attr("vector-effect", "non-scaling-stroke") // from http://stackoverflow.com/questions/10357292/how-to-make-stroke-width-immune-to-the-current-transformation-matrix
		.style("fill", function(d) {return graph.laneColors[numThreads];})
		.style("fill-opacity", "0.3");
}

TimelineGraph.prototype.createTimelineLabels = function(data, className) {
	var graph = this;
	this.timelineGraph.append("g").selectAll("." + className)
		.data(data)
		.enter().append("text")
		.text(graph.getText)
		.attr("x", function(d) {return (graph.xScale(d.start) + graph.xScale(d.end))/2;})
		.attr("y", function(d) {return graph.yScale(d.lane + .5);})
		.attr("dy", ".5ex")
		.attr("id", function(d) {return "" + d.id + graph.nameSuffix + "-label"})
		.attr("class", className)
		.on("click", function(d) {return graph.timelineNodeClickHandler(d)})
		.attr("text-anchor", "middle");
};

TimelineGraph.prototype.createTicTocRegionLabels = function(ticTocRegions, className) {
	var graph = this;
	this.timelineGraph.append("g").selectAll("." + className)
		.data(ticTocRegions)
		.enter().append("text")
		.text(function(d) {if (d.name != "all") {return d.name} else {return ""}})
		.attr("x", function(d) {return (graph.xScale(d.start) + graph.xScale(d.end))/2;})
		.attr("y", function(d) {return graph.yScale(graph.executionProfile.numThreads + .5);})
		.attr("dy", ".5ex")
		.attr("id", function(d) {return "" + d.id + graph.nameSuffix + "-label"})
		.attr("class", className)
		.attr("text-anchor", "middle");
};

TimelineGraph.prototype.getText = function(d) {
	return d.displayText;
};

TimelineGraph.prototype.getClassNameForRect = function(d) {
	if (this.config.syncNodeRegex.test(d.name)) {
		return "sync-node";
	}

	return "kernelNode";
};

TimelineGraph.prototype.getNodeName = function(name) {
	var m = name.match(this.re_partition);
	if (m) { return m[1]; }

	m = name.match(this.re_header)
	if (m) { return m[1]; }

	return name;
};

TimelineGraph.prototype.getRectFill = function(d) {
	if (this.config.syncNodeRegex.test(d.name)) {
		return "grey";
	}

	return this.laneColors[d.lane];
};

TimelineGraph.prototype.timelineNodeClickHandler = function(tNode) {
	var nodeType = tNode.type;
	if (nodeType == "sync") {
		this.config.populateSyncNodeInfoTable(tNode);
	} else if (nodeType == "execution") {
		this.config.populateKernelInfoTable(tNode);
		this.config.markGraphNode(tNode.node.id);
		this.config.markNeighborsOnGraph(tNode.node.id);
		//this.config.markGraphNode(tNode.id);
		//this.config.markNeighborsOnGraph(tNode.id);

		//var dNode = this.dependencyData.nodes[tNode.id];
		var sc = tNode.node.sourceContext;
		this.config.highlightLineInEditor(sc.file, sc.line);
	}
};

TimelineGraph.prototype.dblClickHandler = function(tNode) {
	if (tNode.childNodes.length > 0) {
		var isStackChanged = false;
		if ((tNode.parentId == -1) && (this.stackOfHiddenNodes.length > 0)) {
			var selector = this.stackOfHiddenNodes[0][0];
			$(selector).show();
			$(selector + "-label").show();
			this.stackOfHiddenNodes.length = 0; // clear the array
			isStackChanged = true;
		}

		var childNodes = tNode.childNodes.concat(tNode.syncNodes);
		var rectSelector = "#" + d3.event.target.id;
		this.stackOfHiddenNodes.push([rectSelector, tNode]);
		$(rectSelector).hide();
		$(rectSelector + "-label").hide();

		this.removeChildNodesAndLabels();
		this.createTimelineNodes(childNodes, this.timelineChildNodeClass);
		this.createTimelineLabels(this.filterNodesEligibleForLabels(childNodes), this.timelineChildNodeLabelClass);

		isStackChanged = true;
	}

	if (isStackChanged) this.updateHiddenNodeList();
};

TimelineGraph.prototype.updateHiddenNodeList = function() {
	var sel = $(this.timelineLevelSelectorId);
	for (var i = sel[0].options.length - 1; i >= 0; i--) sel[0].remove(i);
	if (this.stackOfHiddenNodes.length > 0) {
		for (var i = this.stackOfHiddenNodes.length; i >= 0; i--) {
			sel.append($("<option/>", {
				value: i,
				text: "Level " + i
			}));
		}
	}
};

TimelineGraph.prototype.displayNode = function(tNode) {
	var nodesToDisplay = [];
	if (tNode.parentId == -1) {
		nodesToDisplay = [tNode];
	} else {
		nodesToDisplay = tNode.parent.childNodes;
	}

	this.createTimelineNodes(nodesToDisplay, this.timelineChildNodeClass);
	this.createTimelineLabels(this.filterNodesEligibleForLabels(nodesToDisplay), this.timelineChildNodeLabelClass);
};

TimelineGraph.prototype.filterNodesEligibleForLabels = function(tNodes) {
	var minDurationReqForDisplayingLabel = 0.05 * this.timelineData.totalAppTime;
	return tNodes.filter(function(d) {return (d.end - d.start) >= minDurationReqForDisplayingLabel});
};

TimelineGraph.prototype.timelineScopeSelHandler = function(event) {
	graph = event.data.graph;
	graph.removeChildNodesAndLabels();

	var selectedLevel = parseInt($(this).val());
	if (selectedLevel == 0) {
		var rectSelector = graph.stackOfHiddenNodes[selectedLevel][0];
		$(rectSelector).show();
		$(rectSelector + "-label").show();
	} else {
		var tNode = graph.stackOfHiddenNodes[selectedLevel][1];
  		graph.displayNode(tNode);
	}

	graph.stackOfHiddenNodes.length = selectedLevel;
	graph.updateHiddenNodeList();
};

TimelineGraph.prototype.removeChildNodesAndLabels = function() {
	$(toClassSelector(this.timelineChildNodeClass)).remove();
	$(toClassSelector(this.timelineChildNodeLabelClass)).remove();
};

TimelineGraph.prototype.zoom = function(scale) {
	function changeWidthOfNodes(selector) {
		d3.selectAll(selector)
		  .attr("x", function(d) {return x(d.start)})
		  .attr("width", function(d) {return x(d.end) - x(d.start);});
	}

	function repositionNodeLabels(selector) {
		d3.selectAll(selector)
		  .attr("x", function(d) {return ((x(d.start) + x(d.end))/2);});		
	}

	var x = this.xScale;
	this.xScale.range([scale * this.initialXRange[0], scale * this.initialXRange[1]]);

	// Resize the SVG
	d3.select(toIdSelector(this.svgId)).attr("width", scale * this.chartWidth);

	// Resize the lane lines
	d3.selectAll(toClassSelector(this.laneLineClass)).attr("x2", scale * this.chartWidth);

	// Resize all the kernel and sync nodes currently displayed
	changeWidthOfNodes(toClassSelector(this.timelineNodeClass));
	changeWidthOfNodes(toClassSelector(this.timelineChildNodeClass));

	// Reposition all the kernel labels currently displayed
	repositionNodeLabels(toClassSelector(this.timelineNodeLabelClass));
	repositionNodeLabels(toClassSelector(this.timelineChildNodeLabelClass));
};