
function createStackGraph(parentDivId, data, xScale) {
	//var margin = {top: 0, right: 0, bottom: 0, left: 20}
	var margin = {top: 0, right: 15, bottom: 0, left: 120}
    var parentDiv = $(parentDivId)
    var width = parentDiv.width() * 2.5
    var height = parentDiv.height() - margin.top - margin.bottom

	/*
	var x = d3.scale.linear()
	    	  .range([0, width]);
	x.domain(d3.extent(data, function(d) { return d.time; }))
	
	*/

	var x = xScale

	var y = d3.scale.linear()
	    	  .range([height, 0])
			  
	var z = d3.scale.category20c();

	var yAxis = d3.svg.axis()
	    .scale(y)
	    .orient("left");

	var stack = d3.layout.stack()
	    .offset("zero")
	    .order("reverse")
	    .values(function(d) { return d.values })
	    .x(function(d) { return d.time })
	    .y(function(d) { return d.value })

	var nest = d3.nest()
	    .key(function(d) { return d.key })

	var area = d3.svg.area()
	    .interpolate("cardinal")
	    .x(function(d) { return x(d.time) })
	    .y0(function(d) { return y(0) })
	    .y1(function(d) { return y(d.y) })

	var svg = d3.select(parentDivId).append("svg")
	    .attr("width", width + margin.left + margin.right)
	    .attr("height", height + margin.top + margin.bottom)
	    .append("g")
	    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")


	var seriesArr = nest.entries(data) // An array of objects. Each obj would correspond to one series: ie, Max Memroy, Tot Memory, etc.
	var layers = stack(seriesArr)

	y.domain([0, d3.max(data, function(d) { return d.y; })])

	var series = svg.selectAll(".series")
		.data(layers)
		.enter()
		.append("g")
		.attr("class", ".series")

	series.append("path")
	  	.attr("class", "layer")
	  	.attr("d", function(d) { return area(d.values); })
	  	.style("fill", function(d, i) { return z(i); })

   	var markerDimension = 4
   	series.selectAll(".point")
		.data(function(d) {return d.values}) // 'd' => array of data points for a given series
		.enter()
		.append("rect")
		.attr("class", "point")
		.attr("x", function(d) { return x(d.time) - markerDimension/2}) // 'd' => a single data point within a given series
		//.attr("y", function(d) { return y(d.y) - markerDimension/2})
		.attr("y", function(d) { return y(d.y)})
		.attr("width", markerDimension)
		.attr("height", markerDimension)
		.style("fill", "orange")
		.on("mouseenter", function (d) { showPopover.call(this, d); })
   		.on("mouseleave",  function (d) { removePopovers(); })

   	function removePopovers () {
	  $('.popover').remove()
	}

	function showPopover (d) {
	  $(this).popover({
	    //title: d.key,
	    placement: 'auto top',
	    container: 'body',
	    trigger: 'manual',
	    html : true,
	    content: function() { 
	      return d.key + ": " + d.value + 
	             "<br/>Time: " + d.time
	  }});
	  $(this).popover('show')
	}

	/*
	svg.append("g")
	  .attr("class", "y axis")
	  .call(yAxis)
	*/
}

/*
var data = []
data.push({key: "Max Memory", value: 100, time: 100})
data.push({key: "Total Memory", value: 32, time: 100})
data.push({key: "Free Memory", value: 20, time: 100})
data.push({key: "Max Memory", value: 100, time: 200})
data.push({key: "Total Memory", value: 54, time: 200})
data.push({key: "Free Memory", value: 42, time: 200})
data.push({key: "Max Memory", value: 100, time: 300})
*/