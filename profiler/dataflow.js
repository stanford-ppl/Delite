
function createDataFlowGraph(graphId, graphParentId, colaObj, destinationDivElem, dataModel, viewState, config) {
	// Points regarding the hierarchical display of DEG nodes:
	// (i)  Every graph view would correspond to a certain parentId.
	//		If this is -1, its the top-level view. If we're within WhileLoop, the parentId would
	//		correspond to the id of the WhileLoop
	// (ii)	Only nodes whose parentId == the graphParentId can have non-empty displayInputs and displayOutputs
	// (iii)We maintain a cache of all views generated so far. Following are the data points that's cached for each view:
	//			(a) nodesToDisplay and nodeIdToDisplayIndex
	//			(b) dislpayInputs and displayOutputs of each node 
	//			(c) graphId of the view. This is used to hide/show each view

	hljs.initHighlightingOnLoad();
	var cola = colaObj.d3adaptor();
	var nodes = dataModel["nodes"]
	var nodeNameToId = dataModel["nodeNameToId"]

	// TODO: Refactor this function
	function computeDisplayAttrsOfNodes(nodes) {
		function mapToTopLevelParents(n, attr) {
			return n[attr].map(function(n) {return getTopLevelParent(nodes[n], nodes)})
		}

		function mapChildNodesDepsToParent(n1, childNodes) {
			if(childNodes) {
				childNodes.forEach(function(n2) {
					n1.displayInputs = n1.displayInputs.concat(mapToTopLevelParents(n2, "inputs"))
					n1.displayOutputs = n1.displayOutputs.concat(mapToTopLevelParents(n2, "outputs"))
				})
			}
		}

		var nodesToDisplay = []
		nodes.filter(function(n) {return n.type != "InternalNode"}).forEach(function(n) {
			if (n.parentId == graphParentId) {
				console.log(n)
				nodesToDisplay.push(n)
				n.isPeripheralNodeForDisplay = false
				n.displayInputs = mapToTopLevelParents(n, "inputs")
				n.displayOutputs = mapToTopLevelParents(n, "outputs")

				if (n.type == "WhileLoop") {
					mapChildNodesDepsToParent(n, n.condOps)
					mapChildNodesDepsToParent(n, n.bodyOps)
				} else if (n.type == "Conditional") {
					mapChildNodesDepsToParent(n, n.condOps)
					mapChildNodesDepsToParent(n, n.thenOps)
					mapChildNodesDepsToParent(n, n.elseOps)
				}
			} else {
				n.isPeripheralNodeForDisplay = true
				n.displayInputs = []
				n.displayOutputs = []
			}

			n.displayInputs = n.displayInputs.filter(function(i) {return i != n.id})
			n.displayOutputs = n.displayOutputs.filter(function(i) {return i != n.id})

			$.unique(n.displayInputs)
			$.unique(n.displayOutputs)
		})

		// Adding peripheral nodes, ie, the ones that are inputs/outputs of nodesToDisplay
		if (graphParentId != -1) { // optimization
			var tmp = []
			nodesToDisplay.forEach(function(n) {
				var inputs = n.displayInputs.map(function(i) {return nodes[i]})
				inputs.forEach(function(n) {if (n.parentId != graphParentId) {n.displayOutputs.push(n.id)}})
				tmp = tmp.concat(inputs)

				var outputs = n.displayOutputs.map(function(i) {return nodes[i]})
				outputs.forEach(function(n) {if (n.parentId != graphParentId) {n.displayInputs.push(n.id)}})
				tmp = tmp.concat(outputs)
			})

			$.unique(tmp)
			nodesToDisplay = nodesToDisplay.concat(tmp)
		}

		return {"nodes": nodes, "nodesToDisplay": nodesToDisplay}
	}

	function getTopLevelParent(n, nodes) {
		var parent = n
		while ((parent.parentId != -1) && (parent.parentId != graphParentId)) {
			parent = nodes[parent.parentId]
		}

		console.log("Parent: " + n.name + " => " + parent.name)
		return parent.id
	}

	function mapNodeIdToDisplayIndex(nodesToDisplay) {
		var nodeIdToDisplayIndex = {}
		nodesToDisplay.forEach(function(n, i) {nodeIdToDisplayIndex[n.id] = i})

		return nodeIdToDisplayIndex
	}

	function computeEdges(nodes, nodeIdToDisplayIndex) {
		var edges = []
		nodes.forEach(function (n) {
			var id = toDisplayIndex(n.id)
			n.displayInputs.forEach(function(m) {
				edges.push({source: toDisplayIndex(m), target: id})
			})
		})

		return edges
	}

	function isNestedNode(n) {
		return ((n.type == "WhileLoop") || (n.type == "Conditional"))
	}

	function doubleClickHandler(d) {
		if (isNestedNode(d)) {
			$("#" + graphId).remove()
			var subGraphId = "g_" + d.name
			createDataFlowGraph(subGraphId, d.id, colaObj, destinationDivElem, dataModel, viewState, config)
		}
	}

	var res = computeDisplayAttrsOfNodes(nodes)
	nodes = res.nodes
	var nodesToDisplay = res.nodesToDisplay
	var nodeIdToDisplayIndex = mapNodeIdToDisplayIndex(nodesToDisplay)
	var edges = computeEdges(nodesToDisplay, nodeIdToDisplayIndex)

	console.log(nodes)
	console.log(nodesToDisplay)
	console.log(nodeIdToDisplayIndex)
	console.log(edges)

	$(".dataflowSvg").remove() // Removing DEG graphs generated in previous debug sessions, if any
	var graph = d3.select(destinationDivElem).append("svg")
		.attr("class", 'dataflowSvg')
		.attr("id", graphId)
		.attr("width", "100%")
		.attr("height", "100%")
		.attr("pointer-events", "all");

	graph.append('rect')
		.attr('class', 'background')
		.attr('width', "100%")
		.attr('height', "100%")
		.on("click", function() {$(".dataflow-kernel").fadeTo(0, 1)})
		.call(d3.behavior.zoom().on("zoom", redraw));

	var graphElements = graph.append('g')

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

	//calcDepthOfNodes()

	var maxTimeTakenByAKernel = nodesToDisplay.map(function(n) {return n.percentage_time}).sort(function(a,b) {return b - a})[1]
	var colorNodeBasedOnTimeTaken = d3.scale.linear()
								    .domain([0, maxTimeTakenByAKernel])
								    .range(["white", "red"]);

	var maxMemUsageByAKernel = nodesToDisplay.map(function(n) {return n.memUsage}).sort(function(a,b) {return b - a})[1]
	var colorNodeBasedOnMemUsage = d3.scale.linear()
							    	.domain([0, maxMemUsageByAKernel])
								    .range(["white", "red"]);						  

	//var constraints = generateConstraints()

	cola
	    .linkDistance(150)
	    .avoidOverlaps(true)
	    .size(getColaDimensions())
	    .nodes(nodesToDisplay)
	    .links(edges)
	    //.flowLayout('y')
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
	    .attr("opacity", getOpacity)
	    .attr("rx", 5).attr("ry", 5)
	    .attr("nodeId", function(d) {return d.id})
	    .on("click", nodeClickHandler)
	    .on("dblclick", doubleClickHandler)
	    .attr("class", "dataflow-kernel")
	    .call(cola.drag);

	var label = graphElements.selectAll(".label")
	    .data(nodesToDisplay)
	    .enter().append("text")
	    .attr("class", "label")
	    .text(function (d) { return d.name; })
	    .on("click", nodeClickHandler)
	    .on("dblclick", doubleClickHandler)
	    .call(cola.drag)
	    .each(function (d) {
	        var b = this.getBBox();
	        var extra = 2 * margin + 2 * pad;
	        d.width = b.width + extra;
	        d.height = b.height + extra;
	    });

	cola = cola.start(20, 20, 20)
	var ticks = 0
	cola.on("tick", function () {
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
	});

	return new controller()

	function toDisplayIndex(nodeId) {
		return nodeIdToDisplayIndex[nodeId]
	}

	function redraw() {
		graphElements.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
	}

	function calcDepthOfNodes() {
	    var startingNodes = nodesToDisplay.filter(function(n) {return (n != undefined) && (n.numInputs == 0) && (n.controlDeps) && (n.antiDeps)})
	    startingNodes.forEach(function(n1) {
	        n1.depth = 0
	        getChildren(n1).forEach(function(n2) { updateDepth(n2, 1)})
	    })
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
	/*
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
	*/

	function colorNodeBasedOnDataDeps(n) {
		if (n.isPeripheralNodeForDisplay) {
			return "gray"
		}

		var numOutputs = n.displayOutputs.length
	    if (n.displayInputs.length > 0) {
	        if (numOutputs > 0){
	            return "orange"
	        }
	        return "red"
	    } else {
	        if (numOutputs > 0) {
	            return "green"
	        } else {
	            return "lightblue"
	        }
	    }
	}

	function getOpacity(d) {
		if (d.isPeripheralNodeForDisplay) {
			return 0.2
		}

		return 1
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

	function getColaDimensions() {
		var p = $('.dataflowSvg').parent()
		return [p.width(), p.height()];
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

	/*
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
	*/

	function getNeighbors(node) {
		var neighbors = []
		neighbors = neighbors.concat(node.displayInputs)
		neighbors = neighbors.concat(node.displayOutputs)

		return neighbors
	}

	function highlightNodes(nodeIds) {
		$(".dataflow-kernel").fadeTo(0, 0.1)
		var s = nodeIds.reduce(function(p,c,i,a) {return p + "[nodeId=" + c + "],"}, "")
		s = s.substring(0,s.length - 1)
		$(s).fadeTo(0, 1)
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
			if (scheme == "datadeps") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnDataDeps(d)})
			} else if (scheme == "time") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnTimeTaken(d.percentage_time)})
			} else if (scheme == "memUsage") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnMemUsage(d.memUsage)})
			}
		}
	}
}

