
// ==============================================
//  The dependency graph view controller
// ==============================================

function DependencyGraph(parentDivId, nodes, config) {
	hljs.initHighlightingOnLoad();
	this.cola = cola.d3adaptor();
	this.graphElements = {}
	this.parentDivId = parentDivId;
	this.nodes = nodes;
	this.displayNodes = [];
	this.nodeIdToDisplayIndex = {};
}

// ====================
// Public functions
// ====================

DependencyGraph.prototype.init = function() {
	function redraw() {
		graphElements.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
	}

	var graph = d3.select(this.parentDivId).append("svg")
		.attr('class', 'dataflowSvg')
		.attr("width", "100%")
		.attr("height", "100%")
		.attr("pointer-events", "all"); 

	var zoom = d3.behavior.zoom();
	zoom.on("zoom", redraw);
	zoom.scale(0.3);

	graph.append('rect')
		.attr('class', 'background')
		.attr('width', "100%")
		.attr('height', "100%")
		.on("click", function() {$(".dataflow-kernel").fadeTo(0, 1)})
		.call(zoom);

	this.graphElements = graph.append('g').attr("transform", "scale(0.3)");

	graph.append('svg:defs').append('svg:marker')
		.attr('id', 'end-arrow')
		.attr('viewBox', '0 -5 10 10')
		.attr('refX', 8)
		.attr('markerWidth', 6)
		.attr('markerHeight', 6)
		.attr('orient', 'auto')
		.append('svg:path')
		.attr('d', 'M0,-5L10,0L0,5L2,0')
		.attr('stroke-width', '0px')
		.attr('fill', '#000');
};

DependencyGraph.prototype.displayDependenciesOf = function(nodeId) {
	function addNeighborsToDisplayList(n1, distance) {
		if (distance <= MAX_NEIGHBOR_DISTANCE) {
			n1.inputs.forEach(function(n2) {
				graph.addNodeToDisplayList(n2);
				graph.addEdgeToDisplayList(n2, n1.id);
				addNeighborsToDisplayList(graph.nodes[n2], distance + 1)
			});

			n1.outputs.forEach(function(n2) {
				graph.addNodeToDisplayList(n2);
				graph.addEdgeToDisplayList(n1.id, n2);
				addNeighborsToDisplayList(graph.nodes[n2], distance + 1)
			});
		}
	}

	var MAX_NEIGHBOR_DISTANCE = 5;
	var graph = this;

	this.displayNodes = [];
	this.edges = [];
	this.nodeIdToDisplayIndex = {}
	this.addNodeToDisplayList(nodeId);

	console.log("Neighbors adding...")
	addNeighborsToDisplayList(this.nodes[nodeId], 1)
	console.log("Neighbors adding... DONE")
	//$.unique(graph.displayNodes)
	//console.log("Found the unique elements")
	console.log(graph.displayNodes)
	console.log("# of displayNodes: " + graph.displayNodes.length)
	var l = Object.keys(graph.nodeIdToDisplayIndex)
	$.unique(graph.edges)
	console.log("# of keys: " + l.length)
	console.log(graph.nodeIdToDisplayIndex)
	console.log("id for 10: " + graph.nodeIdToDisplayIndex[10])
	/*
	var n1 = this.nodes[nodeId];
	var graph = this
	n1.inputs.forEach(function(n2) {
		graph.addNodeToDisplayList(n2);
		graph.addEdgeToDisplayList(n2, n1.id);
	});

	n1.outputs.forEach(function(n2) {
		graph.addNodeToDisplayList(n2);
		graph.addEdgeToDisplayList(n1.id, n2);
	});*/

	this.update();
};

DependencyGraph.prototype.appendDependenciesOf = function(n) {

};

DependencyGraph.prototype.colorNodes = function(nodeColorSelector) {

};

// ====================
// Helper functions
// ====================

DependencyGraph.prototype.update = function() {
	console.log("Calling update...")
	this.clear();
	console.log("000")
	console.log(this.displayNodes)
	console.log(this.edges)
	

	this.cola
	    .linkDistance(150)
	    .avoidOverlaps(true)
	    .size(this.getColaDimensions())
	    .nodes(this.displayNodes)
	    .links(this.edges)
	    .jaccardLinkLengths();
	
	console.log("zzz")
	var link = this.graphElements.selectAll(".link")
	    .data(this.edges)
	    .enter().append("line")
	    .attr("class", "link");
	console.log("111")
	
	var node = this.graphElements.selectAll(".kernel-node")
	    .data(this.displayNodes)
		.enter().append("rect")
	    .attr("fill", colorNodeBasedOnDataDeps)
	    .attr("rx", 5).attr("ry", 5)
	    .attr("id", function(d) {return "dfg-" + d.id})
	    .on("click", this.nodeClickHandler)
	    .attr("class", "dataflow-kernel kernel-node")
	    .call(this.cola.drag);
	console.log("222")

	var margin = 6, pad = 12;
	var label = this.graphElements.selectAll(".label")
	    .data(this.displayNodes)
	    .enter().append("text")
	    .attr("class", "label")
	    .text(function (d) { return d.name; })
	    .on("click", this.nodeClickHandler)
	    .call(this.cola.drag)
	    .each(function (d) {
	        var b = this.getBBox();
	        var extra = 2 * margin + 2 * pad;
	        d.width = b.width + extra;
	        d.height = b.height + extra;
	    });
	console.log("333")

	ticks = 0
	graph = this
	this.cola.stop()
	this.cola.start(20, 20, 20).on("tick", function () {
	    node.each(function (d) { d.innerBounds = d.bounds.inflate(-margin); })
	        .attr("x", function (d) { return d.innerBounds.x; })
	        .attr("y", function (d) { return d.innerBounds.y; })
	        .attr("width", function (d) { return d.innerBounds.width(); })
	        .attr("height", function (d) { return d.innerBounds.height(); });

	    link.each(function (d) {
		        vpsc.makeEdgeBetween(d, d.source.innerBounds, d.target.innerBounds, 5);})
	        .attr("x1", function (d) { return d.sourceIntersection.x; })
	        .attr("y1", function (d) { return d.sourceIntersection.y; })
	        .attr("x2", function (d) { return d.arrowStart.x; })
	        .attr("y2", function (d) { return d.arrowStart.y; });

	    label.attr("x", function (d) { return d.x })
	         .attr("y", function (d) { return d.y + (margin + pad) / 2 });
	    ticks++
	    if (ticks > 5) graph.cola.stop()
	});

	console.log("Calling update... DONE")
};

DependencyGraph.prototype.addNodeToDisplayList = function(nodeId) {
	if (!(nodeId in this.nodeIdToDisplayIndex)) {
		console.log("Adding " + nodeId)
		var n = this.nodes[nodeId];
		this.nodeIdToDisplayIndex[nodeId] = this.displayNodes.length;
		this.displayNodes.push(n);
	}
}

DependencyGraph.prototype.addEdgeToDisplayList = function(srcNodeId, dstNodeId) {
	if ((!srcNodeId) || (!dstNodeId)) {
		console.log("srcNodeId: " + srcNodeId + "  dstNodeId: " + dstNodeId)	
	}

	var graph = this
	this.edges.push({
		source: this.toDisplayIndex(srcNodeId, graph.nodeIdToDisplayIndex),
		target: this.toDisplayIndex(dstNodeId, graph.nodeIdToDisplayIndex)
	})
}

DependencyGraph.prototype.getColaDimensions = function() {
	var p = $('.dataflowSvg').parent()
	return [p.width(), p.height()];
}

DependencyGraph.prototype.toDisplayIndex = function(nodeId, nodeIdToDisplayIndex) {
	console.log("nid: " + nodeId + ",   displayId: " + nodeIdToDisplayIndex[nodeId])
	return nodeIdToDisplayIndex[nodeId]
}

DependencyGraph.prototype.nodeClickHandler = function(node) {
	var sc = node.sourceContext
	config.highlightLineInEditor(sc.file, sc.line)
	config.populateKernelInfoTable(node)
	DependencyGraph.prototype.highlightNeighbors(node)
}

DependencyGraph.prototype.highlightNeighbors = function(node) {
	var arr = getNeighbors(node)
	arr.push(node.id)
	this.highlightNodes(arr)
}

DependencyGraph.prototype.highlightNodes = function(nodeIds) {
	$(".dataflow-kernel").fadeTo(0, 0.1)
	nodeIds.forEach(function(i) {
		$("#dfg-" + i).fadeTo(0, 1)
	})
}

DependencyGraph.prototype.clear = function() {
	$(".kernel-node").remove()
	$(".label").remove()
	$(".link").remove()
}

function getNeighbors(node) {
	var neighbors = []
	if (node.type == "WhileLoop") {
		neighbors = neighbors.concat(node.condOps.map(function(n) {return n.id}))
		neighbors = neighbors.concat(node.bodyOps.map(function(n) {return n.id}))
	} else {
		neighbors = neighbors.concat(node.inputs)
		neighbors = neighbors.concat(node.outputs)
	}

	return neighbors
}

function colorNodeBasedOnDataDeps(n) {
    if (n.numInputs > 0) {
        if (n.numOutputs > 0){
            return "orange"
        }
        return "red"
    } else {    // numInputs == 0
        if (n.numOutputs > 0) {
            return "green"
        } else {
            return "lightblue"
        }
    }
}

function createDataFlowGraph_mod(cola, parentDivId, dataModel, config) {
	hljs.initHighlightingOnLoad();
	var cola = cola.d3adaptor();
	var nodes = dataModel["nodes"]
	var nodeNameToId = dataModel["nodeNameToId"]
	var res = filterNodes(nodes)
	var nodesToDisplay = res.nodesToDisplay
	var nodeIdToDisplayIndex = res.nodeIdToDisplayIndex

	var edges = computeEdges(nodesToDisplay, nodeIdToDisplayIndex)

	var graph = d3.select(parentDivId).append("svg")
		.attr('class', 'dataflowSvg')
		.attr("width", "100%")
		.attr("height", "100%")
		.attr("pointer-events", "all");

	var zoom = d3.behavior.zoom()
	zoom.on("zoom", redraw)
	zoom.scale(0.3)

	graph.append('rect')
		.attr('class', 'background')
		.attr('width', "100%")
		.attr('height', "100%")
		.on("click", function() {$(".dataflow-kernel").fadeTo(0, 1)})
		.call(zoom);

	var graphElements = graph.append('g')
							 //.attr("transform", "scale(0.3)translate(180,0)")
							 .attr("transform", "scale(0.3)")

	graph.append('svg:defs').append('svg:marker')
		.attr('id', 'end-arrow')
		.attr('viewBox', '0 -5 10 10')
		.attr('refX', 8)
		.attr('markerWidth', 6)
		.attr('markerHeight', 6)
		.attr('orient', 'auto')
		.append('svg:path')
		.attr('d', 'M0,-5L10,0L0,5L2,0')
		.attr('stroke-width', '0px')
		.attr('fill', '#000');

	calcDepthOfNodes()

	var maxTimeTakenByAKernel = nodesToDisplay.map(function(n) {return n.percentage_time}).sort(function(a,b) {return b - a})[1]
	var colorNodeBasedOnTimeTaken = d3.scale.linear()
								    .domain([0, maxTimeTakenByAKernel])
								    .range(["white", "red"]);

	var maxMemUsageByAKernel = nodesToDisplay.map(function(n) {return n.memUsage}).sort(function(a,b) {return b - a})[1]
	var colorNodeBasedOnMemUsage = d3.scale.linear()
							    	.domain([0, maxMemUsageByAKernel])
								    .range(["white", "red"]);						  

	var constraints = generateConstraints()

	generateConstraints()

	cola
	    .linkDistance(150)
	    .avoidOverlaps(true)
	    //.flowLayout('y')
	    .size(getColaDimensions())
	    .nodes(nodesToDisplay)
	    .links(edges)
	    //.constraints(constraints)
	    .jaccardLinkLengths()

	var link = graphElements.selectAll(".link")
	    .data(edges)
	    .enter().append("line")
	    .attr("class", "link");

	var margin = 6, pad = 12;
	var node = graphElements.selectAll(".node")
	    .data(nodesToDisplay)
	    .enter().append("rect")
	    .attr("fill", colorNodeBasedOnDataDeps)
	    .attr("rx", 5).attr("ry", 5)
	    .attr("id", function(d) {return "dfg-" + d.id})
	    .on("click", nodeClickHandler)
	    .attr("class", "dataflow-kernel")
	    .call(cola.drag);

	var label = graphElements.selectAll(".label")
	    .data(nodesToDisplay)
	    .enter().append("text")
	    .attr("class", "label")
	    .text(function (d) { return d.name; })
	    .on("click", nodeClickHandler)
	    .call(cola.drag)
	    .each(function (d) {
	        var b = this.getBBox();
	        var extra = 2 * margin + 2 * pad;
	        d.width = b.width + extra;
	        d.height = b.height + extra;
	    });

	var kernel_nodes = $(".dataflow-kernel") // to optimize dom_selection.

	var ticks = 0
	/*
	cola.start(20, 20, 20).on("tick", function () {
	    node.each(function (d) { d.innerBounds = d.bounds.inflate(-margin); })
	        .attr("x", function (d) { return d.innerBounds.x; })
	        .attr("y", function (d) { return d.innerBounds.y; })
	        .attr("width", function (d) { return d.innerBounds.width(); })
	        .attr("height", function (d) { return d.innerBounds.height(); });

	    link.each(function (d) {
		        vpsc.makeEdgeBetween(d, d.source.innerBounds, d.target.innerBounds, 5);})
	        .attr("x1", function (d) { return d.sourceIntersection.x; })
	        .attr("y1", function (d) { return d.sourceIntersection.y; })
	        .attr("x2", function (d) { return d.arrowStart.x; })
	        .attr("y2", function (d) { return d.arrowStart.y; });

	    label.attr("x", function (d) { return d.x })
	         .attr("y", function (d) { return d.y + (margin + pad) / 2 });

	    ticks++
	    if (ticks > 5) {
	    	cola.stop()	
	    }
	});*/

	function filterNodes(nodes) {
		// TODO: We would need to adjust the edges based on the level
		// eg: If x1 depends on x2, which is an inner component of WhileLoop x3,
		// then the edge should be from x3 to x1. Does that sound right?
		var nodeIdToDisplayIndex = {}
		var nodesToDisplay = nodes.filter(function(n) {return (n.type != "InternalNode")})
		nodesToDisplay.forEach(function(n, i) {nodeIdToDisplayIndex[n.id] = i})

		return {"nodesToDisplay": nodesToDisplay, "nodeIdToDisplayIndex": nodeIdToDisplayIndex}
	}

	function toDisplayIndex(nodeId) {
		return nodeIdToDisplayIndex[nodeId]
	}

	function computeEdges(nodes, nodeIdToDisplayIndex) {
		var edges = []
		nodes.forEach(function (n) {
			n.inputs.forEach(function(m) {
				edges.push({source: toDisplayIndex(m), target: toDisplayIndex(n.id)})
			})
		})

		return edges
	}

	function calcDepthOfNodes() {
	    var startingNodes = nodesToDisplay.filter(function(n) {return (n != undefined) && (n.numInputs == 0) && (n.controlDeps) && (n.antiDeps)})
	    startingNodes.forEach(function(n1) {
	        n1.depth = 0
	        getChildren(n1).forEach(function(n2) { updateDepth(n2, 1)})
	    })
	}

	function redraw() {
		graphElements.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
	}

	function getChildren(n) {
	    return n.outputs.map(function(id) {
	    	var displayId = toDisplayIndex(id)
	    	return nodesToDisplay[displayId]
	    });
	}

	function updateDepth(node, depth) {
	    if (node.depth < depth) {
	        node.depth = depth
	        getChildren(node).forEach(function (n) {
	            updateDepth(n, depth + 1)
	        })
	    }
	}

	// TODO: User should have the option to choose whether to include datadeps, antideps, control deps, etc. in color coding
	//		 Need to rewrite the function to consider the currently selected mode and counts of all these 3 types of deps 
	//		 when determining color
	function colorNodeBasedOnDataDeps(n) {
	    if (n.numInputs > 0) {
	        if (n.numOutputs > 0){
	            return "orange"
	        }
	        return "red"
	    } else {    // numInputs == 0
	        if (n.numOutputs > 0) {
	            return "green"
	        } else {
	            return "lightblue"
	        }
	    }
	}

	function colorNodeBasedOnType(type) {
		switch(type) {
			case 'MultiLoop'  : return "red"
			case 'WhileLoop'  : return "orange"
			case 'SingleTask' : return "lightblue"
			case 'Conditional': return "green"
			default			  : return "white"
		}
	}

	function generateConstraints() {
		var constraints = []
	    edges.map(function(e) {return {source: nodesToDisplay[e.source], target: nodesToDisplay[e.target]}})
	         .forEach(function(e) {
	            var s = e.source
	            var t = e.target
	            var minDiffInY = (t.depth - s.depth) * 15
	            constraints.push({"axis": "y", "left": toDisplayIndex(s.id), "right": toDisplayIndex(t.id), "gap": minDiffInY})
	         })

	    return constraints
	}

	function nodeClickHandler(node) {
		var sc = node.sourceContext
		config.highlightLineInEditor(sc.file, sc.line)
		config.populateKernelInfoTable(node)
		highlightNeighbors(node)
	}

	function highlightNeighbors(node) {
		var arr = getNeighbors(node)
		arr.push(node.id)
		highlightNodes(arr)
	}

	function getNeighbors(node) {
		var neighbors = []
		if (node.type == "WhileLoop") {
			neighbors = neighbors.concat(node.condOps.map(function(n) {return n.id}))
			neighbors = neighbors.concat(node.bodyOps.map(function(n) {return n.id}))
		} else {
			neighbors = neighbors.concat(node.inputs)
			neighbors = neighbors.concat(node.outputs)
		}

		return neighbors
	}

	function highlightNodes(nodeIds) {
		kernel_nodes.fadeTo(0, 0.1)
		nodeIds.forEach(function(i) {
			$("#dfg-" + i).fadeTo(0, 1)
		})
	}

	function getColaDimensions() {
		var p = $('.dataflowSvg').parent()
		return [p.width(), p.height()];
	}

	function controller()
	{
		this.highlightNode = highlightNode;
		this.unhighlightNode = unhighlightNode;
		this.changeColoringScheme = changeColoringScheme;
		this.markNeighbors = markNeighbors

		function highlightNode(nodeId) {
			var n = $(".dataflow-kernel")[toDisplayIndex(nodeId)]
			n.setAttribute("stroke-width", "12px")
		}

		function unhighlightNode(nodeId) {
			var n = $(".dataflow-kernel")[toDisplayIndex(nodeId)]
			n.setAttribute("stroke-width", "0px")
		}

		function markNeighbors(nodeId) {
			var node = nodes[nodeId]
			highlightNeighbors(node)
		}

		function changeColoringScheme(scheme) {
			if (scheme == "dataDeps") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnDataDeps(d)})
			} else if (scheme == "performance") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnTimeTaken(d.percentage_time)})
			} else if (scheme == "memUsage") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnMemUsage(d.memUsage)})
			} else if (scheme == "nodeType") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnType(d.type)})
			}
		}
	}

	return new controller()
}

