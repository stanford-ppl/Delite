
var maxTimeTakenByAKernel = 0;
var maxMemUsageByAKernel = 0;
var colorNodeBasedOnTimeTaken = {};
var colorNodeBasedOnMemUsage = {};

var kernelNodes = undefined;

function highlightNodes(nodeIds) {
	console.log("calling highlightNodes");
	kernelNodes.fadeTo(0, 0.1)
	nodeIds.forEach(function(i) {
		$("#dfg-" + i).fadeTo(0, 1)
	})
}

function createDataFlowGraph(cola, destinationDivElem, nodesToDisplay, edges, viewState, config) {
	hljs.initHighlightingOnLoad();
	var cola = cola.d3adaptor();

	var graph = d3.select(destinationDivElem).append("svg")
		.attr('class', 'dataflowSvg')
		.attr("width", "100%")
		.attr("height", "100%")
		.attr("pointer-events", "all");

	var zoom = d3.behavior.zoom()
	zoom.on("zoom", redraw)
	zoom.scale(0.3)
	//zoom.translate([180,0])

	graph.append('rect')
		.attr('class', 'background')
		.attr('width', "100%")
		.attr('height', "100%")
		.on("click", function() {$(".dataflow-kernel").fadeTo(0, 1)})
		.call(zoom);

	var graphElements = graph.append('g')
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

	//calcDepthOfNodes()					  
	//var constraints = []
	//generateConstraints()

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
	    .on("click", function(d) { highlightDNodeById(d.id); })
	    .attr("class", "dataflow-kernel")
	    .call(cola.drag);

	var label = graphElements.selectAll(".label")
	    .data(nodesToDisplay)
	    .enter().append("text")
	    .attr("class", "label")
	    .text(function (d) { return d.name; })
	    .on("click", function(d) { highlightDNodeById(d.id); })
	    .call(cola.drag)
	    .each(function (d) {
	        var b = this.getBBox();
	        var extra = 2 * margin + 2 * pad;
	        d.width = b.width + extra;
	        d.height = b.height + extra;
	    });

	kernelNodes = $(".dataflow-kernel") // to optimize dom_selection.

	var ticks = 0
	cola.start(10, 10, 10).on("tick", function () {
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

	function toDisplayIndex(nodeId) {
		return nodeId;
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
	    edges.map(function(e) {return {source: nodesToDisplay[e.source], target: nodesToDisplay[e.target]}})
	         .forEach(function(e) {
	            var s = e.source
	            var t = e.target
	            var minDiffInY = (t.depth - s.depth) * 15
	            constraints.push({"axis": "y", "left": toDisplayIndex(s.id), "right": toDisplayIndex(t.id), "gap": minDiffInY})
	         })
	}
	
	function getColaDimensions() {
		var p = $('.dataflowSvg').parent()
		return [p.width(), p.height()];
	}

	function highlightDNodeById(dNodeId) {
		var neighborNodeIds = [];

		function helper(str) {
			if (str != "") {
				str.split(":").forEach( function(s) { neighborNodeIds.push( parseInt(s) ); } )
			}
		}

		function f( dNode ) {
			var sc = dNode.SOURCE_CONTEXT;
			console.log("sc: " + sc);

			var arr = sc.split(":")
			config.highlightLineInEditor(arr[0], parseInt(arr[1]));
			
			neighborNodeIds = [];
			helper( dNode.INPUT_NODE_IDS );
			helper( dNode.OUTPUT_NODE_IDS );
			helper( dNode.CHILD_NODE_IDS );
			neighborNodeIds.push( dNodeId );

			console.log(highlightNodes);
			highlightNodes( neighborNodeIds );
		}

		config.populateKernelInfoTableById( dNodeId );
		config.profileDB.dbDNodeById( dNodeId, f );
	}

	function controller()
	{
		this.highlightNode = highlightNode;
		this.unhighlightNode = unhighlightNode;
		this.changeColoringScheme = changeColoringScheme;
		this.markNeighbors = markNeighbors
		this.highlightDNodeById = highlightDNodeById;

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
				var nodeNameToSummary = profData.executionProfile.nodeNameToSummary;
				graphElements.selectAll(".dataflow-kernel")
			    			.attr("fill", function(d) {
			    			 	var timePct = (d.name in nodeNameToSummary) ? nodeNameToSummary[d.name].totalTime.pct : 0;
			    			 	return colorNodeBasedOnTimeTaken(timePct);
			    			});
			} else if (scheme == "memUsage") {
				var nodeNameToSummary = profData.executionProfile.nodeNameToSummary;
				graphElements.selectAll(".dataflow-kernel")
			    			.attr("fill", function(d) {
			    				var memUsage = (d.name in nodeNameToSummary) ? nodeNameToSummary[d.name].memUsage : 0;
			    			 	return colorNodeBasedOnMemUsage(memUsage);
			    			});
			} else if (scheme == "nodeType") {
				graphElements.selectAll(".dataflow-kernel")
			    			 .attr("fill", function(d) {return colorNodeBasedOnType(d.type)})
			}
		}
	}

	return new controller()
}
	
function setTimeAndMemColorScales() {
	colorNodeBasedOnTimeTaken = d3.scale.linear()
							    .domain([0, maxTimeTakenByAKernel])
							    .range(["white", "red"]);

	colorNodeBasedOnMemUsage = d3.scale.linear()
						    	.domain([0, maxMemUsageByAKernel])
							    .range(["white", "red"]);
}