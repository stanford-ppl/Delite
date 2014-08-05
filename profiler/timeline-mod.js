
function TimelineGraph(parentDivId, data, config) {
	this.parentDivId = parentDivId;
	this.timelineData = data.timelineData;
	this.dependencyData = data.dependencyData;
	this.config = config;
	this.laneColors = null;
	this.timelineGraph = null;
	this.chartWidth = 0;
	this.xScale = null;
	this.yScale = null;
	this.initialXRange = null;
	this.stackOfHiddenNodes = Array();
}

TimelineGraph.prototype.rectHeight = 20;
TimelineGraph.prototype.re_partition = /^(.*)_(\d+)$/;
TimelineGraph.prototype.re_header = /^(.*)_h$/;

TimelineGraph.prototype.draw = function() {
	var items = this.convertDataToTimelineFormat(this.timelineData.timing); 
	var lanes = this.timelineData.lanes;
	var colors = ["orange", "green", "lightskyblue", "red", "brown"];
	this.laneColors = lanes.map(function(l, i) {return colors[i % colors.length]});

	var tmp = this.getAppBeginAndEndTimes(items);
	var	timeBegin = tmp.begin - tmp.duration * 0.01;
	var	timeEnd = tmp.end + tmp.duration * 0.01;

	var parentDiv = $(this.parentDivId);
	var m = [20, 15, 15, 120]; //top right bottom left
	var	chartWidth = parentDiv.width() * 2.5;
	var	chartHeight = parentDiv.height();

	this.chartWidth = chartWidth;

    var initialXRange = [m[3], chartWidth]
	var x = d3.scale.linear()
		.domain([timeBegin, timeEnd + 50])
		.range(initialXRange);

	this.initialXRange = initialXRange;
	this.xScale = x;

	var	numLanes = lanes.length;
	var y = d3.scale.linear()
		.domain([0, numLanes])
		.range([0, chartHeight]);

	this.yScale = y;
	
	var div2 = d3.select(this.parentDivId)
		.append("div")
		.attr("float", "right")
		.attr("class", "timelineWrapper")

	$('timelineWrapper').css("width", "" + parentDiv.width() + "px")

	var chart = div2
		.append("svg")
		.attr("width", chartWidth)
		.attr("height", chartHeight)
		.attr("class", "chart");

	this.timelineGraph = chart.append("g")
		.attr("width", chartWidth)
		.attr("class", "mini");

	//timeline lanes and texts
	this.timelineGraph.append("g").selectAll(".laneLines")
		.data(items)
		.enter().append("line")
		.attr("x1", m[3])
		.attr("y1", function(d) {return y(d.lane + 0.5);})
		.attr("x2", chartWidth)
		.attr("y2", function(d) {return y(d.lane + 0.5);})
		.attr("stroke", "black")
		.attr("class", "laneLine");

	this.timelineGraph.append("g").selectAll(".laneText")
		.data(lanes)
		.enter().append("text")
		.text(function(d) {return d;})
		.attr("x", m[3] - 60)
		.attr("y", function(d, i) {return y(i + .5);})
		.attr("dy", ".5ex")
		.attr("text-anchor", "end")
		.attr("class", "laneText");

	//timeline item rects
	this.createTimelineNodes(items, "node")

	//timeline labels
	var minDurationReqForDisplayingLabel = 0.05 * this.timelineData.totalAppTime
	var eventsWithLabel = items.filter(function(d) {return (d.end - d.start) >= minDurationReqForDisplayingLabel})
	this.createTimelineLabels(eventsWithLabel, "miniLabels")
}

TimelineGraph.prototype.convertDataToTimelineFormat = function(data) {
	var res = []
	for (level in [0]) {
		var runs = data[level]
		for (node in runs) {
			res = res.concat(runs[node])
		}
	}

	return res
}

TimelineGraph.prototype.getAppBeginAndEndTimes = function(items) {
	var appNode = items.filter(function(n) {return n.name == "all"})[0]
	return {"begin": appNode.start, "end": appNode.end, "duration": appNode.duration}
}

TimelineGraph.prototype.createTimelineNodes = function(data, className) {
	var graph = this
	var x = graph.xScale
	var y = graph.yScale

	this.timelineGraph.append("g").selectAll("." + className)
		.data(data)
		.enter().append("rect")
		.attr("class", function(d) {return className + " " + graph.getClassNameForRect(d) + " " + graph.getLevelAttr(d)})
		.attr("x", function(d) {return x(d.start);})
		.attr("y", function(d) {return y(d.lane + .5) - graph.rectHeight/2;})
		.attr("width", function(d) {return x(d.end) - x(d.start);})
		.attr("height", graph.rectHeight)
		.attr("id", function(d) {return d.id})
		.attr("name", function(d) {return graph.getNodeName(d.name)})
		.attr("vector-effect", "non-scaling-stroke") // from http://stackoverflow.com/questions/10357292/how-to-make-stroke-width-immune-to-the-current-transformation-matrix
		.style("fill", function(d) {return graph.getRectFill(d)})
		.on("click", function(d) {return graph.selectNode(d)})
		.on("dblclick", function(d) {return graph.dblClickHandler(d)})
}

TimelineGraph.prototype.createTimelineLabels = function(data, className) {
	var graph = this
	this.timelineGraph.append("g").selectAll("." + className)
		.data(data)
		.enter().append("text")
		.text(graph.getText)
		.attr("x", function(d) {return (graph.xScale(d.start) + graph.xScale(d.end))/2;})
		.attr("y", function(d) {return graph.yScale(d.lane + .5);})
		.attr("dy", ".5ex")
		.attr("id", function(d) {return d.id + "-label"})
		.attr("class", function(d) {return className + " timelineNodeName " + graph.getLevelAttr(d)})
		.on("click", function(d) {return graph.selectNode(d)})
		.attr("text-anchor", "middle")
}

TimelineGraph.prototype.getText = function(d) {
	return d.displayText
}

TimelineGraph.prototype.getClassNameForRect = function(d) {
	if (this.config.syncNodeRegex.test(d.name)) {
		return "sync-node"
	}

	return "timingNode"
}

TimelineGraph.prototype.getLevelAttr = function(d) {
	if ((d.childNodes.length > 0) || (d.syncNodes.length > 0) || (d.parentId >= 0)) {
		return "level-" + d.level
	}

	return ""
}

TimelineGraph.prototype.getNodeName = function(name) {
	var m = name.match(this.re_partition)
	if (m) { return m[1] }

	m = name.match(this.re_header)
	if (m) { return m[1] }

	return name
}

TimelineGraph.prototype.getRectFill = function(d) {
	if (this.config.syncNodeRegex.test(d.name)) {
		return "grey"
	}

	return this.laneColors[d.lane]
}

TimelineGraph.prototype.selectNode = function(d) {
	if (d.type == "sync") {
		this.config.populateSyncNodeInfoTable(d)
	} else if (d.type == "execution") {
		var n = this.dependencyData.nodes[d.id]
		this.config.populateKernelInfoTable(n)

		var id = d.id
		if (n.type == "InternalNode") {
			this.config.markGraphNode(n.parentId)
			this.config.markNeighborsOnGraph(n.parentId)
			n = this.dependencyData.nodes[n.parentId]
		} else {
			this.config.markGraphNode(d.id)
			this.config.markNeighborsOnGraph(d.id)
		}

		var sc = n.sourceContext
		this.config.highlightLineInEditor(sc.file, sc.line)
	}
}

TimelineGraph.prototype.dblClickHandler = function(tNode) {
	if (tNode.childNodes.length > 0) {
		var isStackChanged = false
		if ((tNode.parentId == -1) && (this.stackOfHiddenNodes.length > 0)) {
			var selector = this.stackOfHiddenNodes[0][0]
			$(selector).show()
			$(selector + "-label").show()
			this.stackOfHiddenNodes.length = 0 // clear the array
			isStackChanged = true
		}

		var childNodes = tNode.childNodes.concat(tNode.syncNodes)
		var rectSelector = "#" + d3.event.target.id
		this.stackOfHiddenNodes.push([rectSelector, tNode])
		$(rectSelector).hide()
		$(rectSelector + "-label").hide()

		$(".childNode").remove()
		this.createTimelineNodes(childNodes, "childNode")

		$(".childNodeLabel").remove()
		this.createTimelineLabels(this.filterNodesEligibleForLabels(childNodes), "childNodeLabel")

		isStackChanged = true
	}

	if (isStackChanged) this.updateHiddenNodeList()
}

TimelineGraph.prototype.updateHiddenNodeList = function() {
	var sel = $("#timelineHiddenNodeList")
	for (var i = sel[0].options.length - 1; i >= 0; i--) sel[0].remove(i)
	if (this.stackOfHiddenNodes.length > 0) {
		for (var i = this.stackOfHiddenNodes.length; i >= 0; i--) {
			sel.append($("<option/>", {
				value: i,
				text: "Level " +i
			}))
		}
	}
}

TimelineGraph.prototype.displayNode = function(tNode) {
	var nodesToDisplay = []
	if (tNode.parentId == -1) {
		nodesToDisplay = [tNode]
	} else {
		nodesToDisplay = tNode.parent.childNodes
	}

	this.createTimelineNodes(nodesToDisplay, "childNode")
	this.createTimelineLabels(this.filterNodesEligibleForLabels(nodesToDisplay), "childNodeLabel")
}

TimelineGraph.prototype.filterNodesEligibleForLabels = function(tNodes) {
	var minDurationReqForDisplayingLabel = 0.05 * this.timelineData.totalAppTime
	return tNodes.filter(function(d) {return (d.end - d.start) >= minDurationReqForDisplayingLabel})
}

TimelineGraph.prototype.timelineScopeSelHandler = function(event) {
	$(".childNode").remove()
	$(".childNodeLabel").remove()
	graph = event.data.graph

	var selectedLevel = parseInt($(this).val())
	if (selectedLevel == 0) {
		var rectSelector = graph.stackOfHiddenNodes[selectedLevel][0]
		$(rectSelector).show()
		$(rectSelector + "-label").show()
	} else {
		var tNode = graph.stackOfHiddenNodes[selectedLevel][1]
  		graph.displayNode(tNode)
	}

	graph.stackOfHiddenNodes.length = selectedLevel
	graph.updateHiddenNodeList()
}

TimelineGraph.prototype.zoom = function(scale) {
	var x = this.xScale;
	x.range([scale * this.initialXRange[0], scale * this.initialXRange[1]]);

	d3.selectAll(".timingNode, .sync-node")
	  .attr("x", function(d) {return x(d.start)})
	  .attr("width", function(d) {return x(d.end) - x(d.start);});

	d3.selectAll(".timelineNodeName").attr("x", function(d) {return ((x(d.start) + x(d.end))/2);});
	d3.select(".chart").attr("width", scale * this.chartWidth);
	d3.selectAll(".laneLine").attr("x2", scale * this.chartWidth);
}