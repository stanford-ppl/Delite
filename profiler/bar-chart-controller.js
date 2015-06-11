
var MAX_NODES_TO_DISPLAY_ON_BAR_CHART = 20;

function BarChartController( parentDivId, config ) {
	this.parentDivId = parentDivId;
	this.config = config;
	this.metricToParameters = { };
	this.cache = { };
	this.init();
}

BarChartController.prototype.addParametersForMetric = function( metric, comparisonAttr, getDisplayText, getQuery, onclick ) {
	var params = {
		"comparisonAttr": comparisonAttr,
		"getDisplayText": getDisplayText,
		"getQuery"		: getQuery,
		"onclick"		: onclick
	};

	this.metricToParameters[metric] = params;
};

BarChartController.prototype.init = function() {
	this.addParametersForMetric( "performance", "TOTAL_TIME", getDisplayTextForTime, getQueryForExecSummaries );
	this.addParametersForMetric( "memUsage", "MEM_USAGE", getDisplayTextForMemUsage, getQueryForExecSummaries );

	this.addParametersForMetric( "l2CacheMissRatio", "L2_CACHE_MISS_PCT", getDisplayTextForL2CacheMissRatio, getQueryForKernelMemAccessStats, genericOnClick );
	this.addParametersForMetric( "l3CacheMissRatio", "L3_CACHE_MISS_PCT", getDisplayTextForL3CacheMissRatio, getQueryForKernelMemAccessStats, genericOnClick );

	this.addParametersForMetric( "arrayL1CacheMissRatio", "L1_CACHE_MISS_PCT", getDisplayTextForL1CacheMissRatio, getQueryForArrayCacheAccessStats. genericOnClick );
	this.addParametersForMetric( "arrayL2CacheMissRatio", "L2_CACHE_MISS_PCT", getDisplayTextForL2CacheMissRatio, getQueryForArrayCacheAccessStats, genericOnClick );
	this.addParametersForMetric( "arrayL3CacheMissRatio", "L3_CACHE_MISS_PCT", getDisplayTextForL3CacheMissRatio, getQueryForArrayCacheAccessStats, genericOnClick );
	this.addParametersForMetric( "arrayLocRAMMissRatio", "LOCAL_DRAM_MISS_PCT", getDisplayTextForLocRAMMissRatio, getQueryForArrayCacheAccessStats, genericOnClick );
	this.addParametersForMetric( "arrayRemRAMMissRatio", "REMOTE_DRAM_MISS_PCT", getDisplayTextForRemRAMMissRatio, getQueryForArrayCacheAccessStats, genericOnClick );
};

BarChartController.prototype.display = function( metric ) {
	var p = this.metricToParameters[metric];
	var comparisonAttr = p.comparisonAttr;
	var getDisplayText = p.getDisplayText;
	var onclick = p.onclick;
	var getQuery = p.getQuery;

	if ( metric in this.cache ) {
		var data = this.cache[metric];
		this.displayHelper( data, comparisonAttr, getDisplayText, onclick );
	} else {
		var controller = this;
		var f = function( data ) {
			data = data.slice( 0, MAX_NODES_TO_DISPLAY_ON_BAR_CHART );
			controller.cache[metric] = data;
			controller.displayHelper( data, comparisonAttr, getDisplayText, onclick );
		};

		var query = getQuery( comparisonAttr );
		config.profileDB.fetchMultipleElemsFromDB( query, f );
	}
};

BarChartController.prototype.displayHelper = function( data, comparisonAttr, getDisplayText, onclick ) {
	createBarChart( this.parentDivId, data, comparisonAttr, getDisplayText, this.config, onclick );
};

function getQueryForExecSummaries( comparisonAttr ) {
	return "SELECT * FROM DNodes INNER JOIN ExecutionSummaries ON DNodes.NAME == ExecutionSummaries.NAME ORDER BY " + comparisonAttr + " DESC";
};

function getQueryForKernelMemAccessStats( comparisonAttr ) {
	return "SELECT * FROM KernelMemAccessStats ORDER BY " + comparisonAttr + " DESC";
};

function getQueryForArrayCacheAccessStats( comparisonAttr ) {
	return "SELECT * FROM ArrayCacheAccessStats ORDER BY " + comparisonAttr + " DESC";
};

function getDisplayTextForTime(d) {
	return d.NAME + " (" + getDisplayTextForTimeAbsPctPair( d.TOTAL_TIME, d.TOTAL_TIME_PCT ) + ")";
}

function getDisplayTextForMemUsage(d) { return d.NAME + " (" + memUsageValueToStr( d.MEM_USAGE ) + ")"; }

function getDisplayTextForL1CacheMissRatio(d) { return d.NAME + " (" + d.L1_CACHE_MISS_PCT + "%)";    }
function getDisplayTextForL2CacheMissRatio(d) { return d.NAME + " (" + d.L2_CACHE_MISS_PCT + "%)";    }
function getDisplayTextForL3CacheMissRatio(d) { return d.NAME + " (" + d.L3_CACHE_MISS_PCT + "%)";    }
function getDisplayTextForLocRAMMissRatio(d)  { return d.NAME + " (" + d.LOCAL_DRAM_MISS_PCT + "%)";  }
function getDisplayTextForRemRAMMissRatio(d)  { return d.NAME + " (" + d.REMOTE_DRAM_MISS_PCT + "%)"; }

function getDisplayTextForThreadLevelSync(d) { return d.name + " (" + d.syncTimePct + "%)" }

function genericOnClick( d ) {
	var arr = d.NAME.split(":");
	config.highlightLineInEditor( arr[0], parseInt(arr[1]) );
}


