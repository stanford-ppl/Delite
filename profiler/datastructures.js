
//====================================================
// Global variables
//====================================================

var ticTocRegionId = 0
var tNodeId = 0;

//====================================================
// Data structures
//====================================================

function TNode(name, threadId, start, duration, dNode, config) {
	this.name = name;
	this.lane = threadId;
	this.start = start;
	this.duration = duration;
	this.end = start + duration;
	this.node = dNode;
	//this.id = (dNode) ? dNode.id : undefined;
	this.id = tNodeId++;
	this.level = getTNodeLevel(this);
	this.displayText = getDisplayTextForTimelineNode(this, config.syncNodeRegex);
	this.childNodes = [];
	this.syncNodes = [];
	this.parentId = -1;
	this.dep_thread = ""; // important for sync nodes - specifies the thread the sync was expecting a result from
	this.dep_kernel = ""; // important for sync nodes - specifies the kernel the sync was expecting to complete
	this.execTime = new Time(0,0);
	this.syncTime = new Time(0,0);
	this.ticTocRegions = [];
	this.type = config.syncNodeRegex.test(this.name) ? "sync" : "execution";
}

function TicTocRegion(name, start, duration, numThreads) {
	this.id = ticTocRegionId++;
	this.name = name;
	this.start = start;
	this.totalTime = new Time(duration,0);
	this.end = start + duration;
	this.parent = null;
	this.childNodes = [];
	this.childToPerf = {};
	this.execTimeStats = createThreadLevelTimeStats(numThreads);
	this.syncTimeStats = createThreadLevelTimeStats(numThreads);
}

//====================================================
// Connector
//====================================================

function Server( serverURL ) {
	this.url = serverURL + "?query=" ;
}

Server.prototype.xmlHttpRequest = function( query, parseResponse ) {
  var req = new XMLHttpRequest();
  var reqUrl = this.url + encodeURIComponent( query );
  req.open( 'GET', reqUrl, false );
  req.onreadystatechange = function() { parseResponse( req.responseText ) };
  req.send();
};

//====================================================
// RemoteProfileDB
//====================================================

function RemoteProfileDB( server ) {
	this.server = server;
}

RemoteProfileDB.prototype.processQuery = function ( query, handler, isSingleton ) {
	var f = function(res) {
		var j = JSON.parse( res );
		if ( isSingleton ) { handler(j[0]); }
		else { handler(j); }
	};

	this.server.xmlHttpRequest( query, f );
}

RemoteProfileDB.prototype.fetchSingletonFromDB = function( query, handler ) {
	var f = function(res) {
		var j = JSON.parse( res );
		handler(j[0]);
	};

	this.server.xmlHttpRequest( query, f );
};

RemoteProfileDB.prototype.fetchMultipleElemsFromDB = function( query, handler ) {
	var f = function(res) {
		var j = JSON.parse( res );
		handler(j);
	};

	this.server.xmlHttpRequest( query, f );
};

RemoteProfileDB.prototype.dbDNodeById = function ( dNodeId, handler ) {
	var query = "SELECT * FROM DNodes WHERE ID=" + dNodeId;
	this.fetchSingletonFromDB( query, handler );
};

RemoteProfileDB.prototype.dbDNodeByName = function( dNodeName, handler ) {
	var query = "SELECT * FROM DNodes WHERE NAME='" + dNodeName + "'";
	this.fetchSingletonFromDB( query, handler );
};

RemoteProfileDB.prototype.dbTNodeById = function( tNodeId, handler ) {
	var query = "SELECT * FROM TNodes WHERE id=" + tNodeId;
	this.fetchSingletonFromDB( query, handler );
};

RemoteProfileDB.prototype.dbExecutionSummaryByName = function( name, handler ) {
	var query = "SELECT * FROM ExecutionSummaries WHERE NAME='" + name + "'";
	this.fetchSingletonFromDB( query, handler );
};

RemoteProfileDB.prototype.dbChildTNodes = function( parentTNodeId, handler ) {
	var query = "SELECT * FROM TNodes WHERE parentId=" + parentTNodeId;
	this.fetchMultipleElemsFromDB( query, handler );
};

//====================================================
// RemoteProfileDB
//====================================================

function InMemoryProfileDB(dbRef) {
	this.db = dbRef;
	this.stmt = undefined;
}

InMemoryProfileDB.prototype.fetchSingletonFromDB = function( query ) {
	this.stmt = this.db.prepare( query );
	this.stmt.step();
	return this.stmt.getAsObject();
};

InMemoryProfileDB.prototype.fetchMultipleElemsFromDB = function( query ) {
	var results = [];
	this.stmt = this.db.prepare( query );
	while ( this.stmt.step() ) {
		results.push( this.stmt.getAsObject() );
	}

	return results;
};

InMemoryProfileDB.prototype.dbExecutionSummaryByName = function( name ) {
	return this.fetchSingletonFromDB( "SELECT * FROM ExecutionSummaries WHERE NAME='" + name + "'" );
};

InMemoryProfileDB.prototype.threadCount = function() {
	var res = this.fetchSingletonFromDB( "SELECT * FROM AppData ");
	return res.THREAD_COUNT;
};

InMemoryProfileDB.prototype.ticTocRegionSummaries = function() {
	return this.fetchMultipleElemsFromDB("SELECT * FROM TicTocNodeSummaries");
};

//====================================================
// Helper functions (used only within this file)
//====================================================

function createThreadLevelTimeStats(numThreads) {
    var tidToTime = {};
    for (var tid = 0; tid < numThreads; tid++) {
        tidToTime[tid] = new Time(0, 0);
    }

    return tidToTime;
}