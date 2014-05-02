
function createTimeline(timelineDivClass, profileData, config) {
	// TODO: Rename the variable 'items' 
	var items = getDataInTimelineFormat(profileData.timelineData.timing)
	var rectHeight = 20;

	function getDataInTimelineFormat(data) {
		var res = []
		for (level in data) {
			var runs = data[level]
			for (node in runs) {
				res = res.concat(runs[node])
			}
		}

		return res
	}

	var re_partition = /^(.*)_(\d+)$/
	var re_header = /^(.*)_h$/

	function getDisplayDataForNode(node) {
		var data = [node.name,
					node.target,
					node.type,
					node.num_of_runs,
					node.time,
					node.percentage_time,
					"NA"]
		return data
	}

	function getAppBeginAndEndTimes(items) {
		var appNode = items.filter(function(n) {return n.name == "all"})[0]
		return {"begin": appNode.start, "end": appNode.end, "duration": appNode.duration}
	}
	  
	var lanes = profileData.timelineData.lanes
	var colors = ["orange", "green", "lightskyblue", "red", "brown"]
	var laneColors = lanes.map(function(l, i) {return colors[i % colors.length]})
	var	numLanes = lanes.length
	var tmp = getAppBeginAndEndTimes(items)
	var	timeBegin = tmp.begin - tmp.duration * 0.01
	var	timeEnd = tmp.end + tmp.duration * 0.01

	var parentDiv = $('#timeline')


	var m = [20, 15, 15, 120], //top right bottom left
		chartWidth = parentDiv.width() * 2.5,
		chartHeight = parentDiv.height();

	//scales
	var x = d3.scale.linear()
			.domain([timeBegin, timeEnd + 50])
			.range([m[3], chartWidth]);

	var y = d3.scale.linear()
			.domain([0, numLanes])
			.range([0, chartHeight]);
	
	var div2 = d3.select(timelineDivClass)
				.append("div")
				.attr("float", "right")
				.attr("class", "timelineWrapper")

	$('timelineWrapper').css("width", "" + parentDiv.width() + "px")

	var chart = div2
				.append("svg")
				.attr("width", chartWidth)
				.attr("class", "chart");

	var timelineGraph = chart.append("g")
				.attr("width", chartWidth)
				.attr("class", "mini");

	//timeline lanes and texts
	timelineGraph.append("g").selectAll(".laneLines")
		.data(items)
		.enter().append("line")
		.attr("x1", m[3])
		.attr("y1", function(d) {return y(d.lane + 0.5);})
		.attr("x2", chartWidth)
		.attr("y2", function(d) {return y(d.lane + 0.5);})
		.attr("stroke", "black")
		.attr("class", "laneLine");

	timelineGraph.append("g").selectAll(".laneText")
		.data(lanes)
		.enter().append("text")
		.text(function(d) {return d;})
		.attr("x", m[3] - 60)
		.attr("y", function(d, i) {return y(i + .5);})
		.attr("dy", ".5ex")
		.attr("text-anchor", "end")
		.attr("class", "laneText");

	//timeline item rects
	timelineGraph.append("g").selectAll("nodes")
		.data(items)
		.enter().append("rect")
		.attr("class", getClassNameForRect)
		.attr("level", getLevelAttr)
		.attr("x", function(d) {return x(d.start);})
		.attr("y", function(d) {return y(d.lane + .5) - rectHeight/2;})
		.attr("width", function(d) {return x(d.end) - x(d.start);})
		.attr("height", rectHeight)
		.attr("id", function(d) {return d.id})
		.attr("name", function(d) {return getNodeName(d.name)})
		.attr("title", "rectangle")
		.attr("vector-effect", "non-scaling-stroke") // from http://stackoverflow.com/questions/10357292/how-to-make-stroke-width-immune-to-the-current-transformation-matrix
		.style("fill", getRectFill)
		.on("click", selectNode)

	//timeline labels
	var minDurationReqForDisplayingLabel = 0.05 * profileData.timelineData.totalAppTime
	var eventsWithLabel = items.filter(function(d) {return (d.end - d.start) >= minDurationReqForDisplayingLabel})
	timelineGraph.append("g").selectAll(".miniLabels")
		.data(eventsWithLabel)
		.enter().append("text")
		.text(getText)
		.attr("level", getLevelAttr)
		.attr("x", function(d) {return (x(d.start) + x(d.end))/2;})
		.attr("y", function(d) {return y(d.lane + .5);})
		.attr("dy", ".5ex")
		.attr("title", "sample title")
		.attr("class", "timelineNodeName")
		.on("click", selectNode)
		.attr("text-anchor", "middle");

	function getRectFill(d) {
		if (config.syncNodeRegex.test(d.name)) {
			return "grey"
		}

		return laneColors[d.lane]
	}

	function selectNode(d) {
		if (d.type == "sync") {
			config.populateSyncNodeInfoTable(d)
		} else if (d.type == "execution") {
			var n = profileData.dependencyData.nodes[d.id]
			config.populateKernelInfoTable(n)

			var id = d.id
			if (n.type == "InternalNode") {
				config.markGraphNode(n.parentId)
				config.markNeighborsOnGraph(n.parentId)
				n = profileData.dependencyData.nodes[n.parentId]
			} else {
				config.markGraphNode(d.id)
				config.markNeighborsOnGraph(d.id)
			}

			var sc = n.sourceContext
			config.highlightLineInEditor(sc.file, sc.line)
		}
	}

	function getLevelAttr(d) {
		if ((d.childNodes.length > 0) || (d.syncNodes.length > 0) || (d.parentId >= 0)) {
			return "level-" + d.level
		}

		return ""
	}

	function getNodeName(name) {
		var m = name.match(re_partition)
		if (m) {
			return m[1]
		}

		m = name.match(re_header)
		if (m) {
			return m[1]
		}

		return name
	}

	function getOpacity(d) {
		if (config.syncNodeRegex.test(d.name)) {
			return 0.1
		}

		return 1.0
	}

	function getClassNameForRect(d) {
		if (config.syncNodeRegex.test(d.name)) {
			return "sync-node"
		}

		return "timingNode"
	}

	function getText(d) {
		return d.displayText
	}

	function scroll(numPixels) {
		console.log("Scrolling " + numPixels)
		document.getElementsByClassName("timelineWrapper")[0].scrollLeft = numPixels
	}

	function hideNodes(selector) {
		$(selector).hide()
		//$(selector).css("fill-opacity", "0")
	}

	function showNodes(selector) {
		$(selector).show()
		//$(selector).css("fill-opacity", "1")
	}

	// NOTE: Performs horizontal zoom only
	function zoom(scale) {
		var t = "scale(" + scale + ", 1)"
		d3.selectAll(".timingNode").attr("transform", t)
		d3.selectAll(".sync-node").attr("transform", t)			
		d3.selectAll(".timelineNodeName").attr("x", function(d) {return scale*((x(d.start) + x(d.end))/2);})
		d3.select(".chart").attr("width", scale * chartWidth)
		d3.selectAll(".laneLine").attr("x2", scale * chartWidth)
		//scroll(scale * $(".timelineWrapper").scrollLeft())
	}

	function hideSyncNodes() {
		$(".sync-node").hide()
	}

	function showSyncNodes() {
		$(".sync-node").show()
	}

	function highlightNodesByName(name) {
		name = getNodeName(name)
		d3.selectAll("[name=" + name + "]").style("stroke-width", "2")
	}

	function unhighlightNodesByName(name) {
		name = getNodeName(name)
		d3.selectAll("[name=" + name + "]").style("stroke-width", "1")
	}

	function scrollToNode(name) {
		var lst = profileData.timelineData.timing[name]
		if (lst) {
			var d = lst[0]
			var xCoord = x(d.start)
			var scrollAmt = xCoord - 500
			if (scrollAmt < 0) scrollAmt = 0
			scroll(scrollAmt)
		} else {
			console.log(profileData.timelineData.timing)
		}
	}

	function controller() {
		this.hideNodes = hideNodes
		this.showNodes = showNodes
		this.zoom = zoom
		this.hideSyncNodes = hideSyncNodes
		this.showSyncNodes = showSyncNodes	
		this.scroll = scroll
		this.scrollToNode = scrollToNode
		this.highlightNodesByName = highlightNodesByName
		this.unhighlightNodesByName = unhighlightNodesByName
	}

	return new controller()
}

// Code for displaying tooltip tables

/*
var table = d3.select(timelineDivClass).append("table")
    .attr("class", "tooltip")
    .attr("position", "absolute")
    .style("opacity", 0);

table.append("col")
	.style("width", "60%")

table.append("col")
	.style("width", "40%")

var columns = ["name", "value"]
var datapoints = ["name", "target", "type", "# runs", "time", "% of tot time", "mem_alloc"]
var valuesOfDataPoints = ["Generic", "Scala", "MultiLoop", "3", "40s", "30%", "164 MB"]
var data = []
for (i in datapoints) {
	data.push({name: datapoints[i], value: valuesOfDataPoints[i]})
}
*/

/*
function tabulate(data, columns) {
	// In the next statement, any non-empty array can be used as input to 'data'
	// Refer to http://stackoverflow.com/questions/14514776/updating-an-html-table-in-d3-js-using-a-reusable-chart
	table.selectAll('thead').data([0]).enter().append('thead');
	var thead = table.select('thead');

	table.selectAll('tbody').data([0]).enter().append('tbody');
	var tbody = table.select('tbody');

	// create a row for each object in the data
	var rows = tbody.selectAll("tr")
		.data(data)
		.enter()
		.append("tr");

	// create a cell in each row for each column
	var cells = rows.selectAll("td")
		.data(function(row) {
	    	return columns.map(function(column) {
	        	return {column: column, value: row[column]};
	    	});
		})
		.enter()
		.append("td")
		.attr("class", "tooltipValue")
	  	.text(function(d) { return d.value; });

	return table;
}
*/

/*
function mousemove(d) {
    d3.select("table")
      .style("left", d3.select(this).attr("x") + "px")     
	  .style("top", (parseFloat(d3.select(this).attr("y")) + rectHeight + 1) + "px");
}

	function mouseover(p) {
  	d3.select("table").transition()
      .duration(500)
      .style("opacity", 1);
  	d3.selectAll(".row text").classed("active", function(d, i) { return i == p.y; });
  	d3.selectAll(".column text").classed("active", function(d, i) { return i == p.x; });

  	//tabulate(data, columns)
	}

	function mouseout() {
  	d3.select(".tooltip").transition()
      .duration(500)
      .style("opacity", 1e-6);
	}
*/