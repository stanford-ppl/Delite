
function createGCStatsGraph(parentDivId, gcStats, xScale, config) {
	var margin = {top: 0, right: 0, bottom: 0, left: 20}
    var parentDiv = $(parentDivId)
    var width = parentDiv.width() * 2.5
    var height = parentDiv.height() - margin.top - margin.bottom

	var x = xScale

	var svg = d3.select(parentDivId).append("svg")
	    .attr("width", width + margin.left + margin.right)
	    .attr("height", height + margin.top + margin.bottom)
	    .append("g")
	    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

   	var annotationMarkers = svg.selectAll(".annotation")
   		.data(gcStats)
   		.enter()
   		.append("rect")
   		.attr("class", "annotation")
   		.attr("x", function(d) {return x(d.start)})
   		.attr("y", 0)
   		.attr("width", function(d) {return x(d.start + d.duration) - x(d.start) })
   		.attr("height", height)
   		.style("fill", getFill)
   		.style("stroke", "black")
   		.style("stroke-width", 1)
   		.on("click", clickHandler)

	function getFill(d) {
		switch(d.type) {
			case MAJOR_GC: return "red"
			case MINOR_GC: return "purple"
			default      : console.error("Unexpected value for GC Event type")
		}
	}
	
	var previouslyClickedNode = 0;
	function clickHandler(d, i) {
		var data = [];
		var y = d.youngGenStats.sizeStats;
		data.push(["Young Gen", y.beforeGC, y.afterGC, y.committed]);

		if (d.type == MAJOR_GC) {
			var o = d.oldGenStats.sizeStats;
			data.push(["Old Gen", o.beforeGC, o.afterGC, o.committed]);
		} else {
			data.push(["Old Gen", "N/A", "N/A", "N/A"]);
		}

		var h = d.heapStats;
		data.push(["Heap", h.beforeGC, h.afterGC, h.committed]);
		
		config.populateGCEventInfoTable(data);
		$(".annotation")[previouslyClickedNode].style["stroke-width"] = "1px"
		$(".annotation")[i].style["stroke-width"] = "5px"
		previouslyClickedNode = i;
	}
}