
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

function ProfileDB(dbRef) {
	this.db = dbRef;
	this.stmt = undefined;
}

ProfileDB.prototype.fetchSingletonFromDB = function( query ) {
	this.stmt = this.db.prepare( query );
	this.stmt.step();
	return this.stmt.getAsObject();
};

ProfileDB.prototype.fetchMultipleElemsFromDB = function( query ) {
	var results = [];
	this.stmt = this.db.prepare( query );
	while ( this.stmt.step() ) {
		results.push( this.stmt.getAsObject() );
	}

	return results;
};

ProfileDB.prototype.dbDNodeById = function ( dNodeId ) {
	return this.fetchSingletonFromDB( "SELECT * FROM DNodes WHERE ID=" + dNodeId );
};

ProfileDB.prototype.dbDNodeByName = function( dNodeName ) {
	return this.fetchSingletonFromDB( "SELECT * FROM DNodes WHERE NAME='" + dNodeName + "'");
};

ProfileDB.prototype.dbTNodeById = function( tNodeId ) {
	return this.fetchSingletonFromDB( "SELECT * FROM TNodes WHERE id=" + tNodeId );
};

ProfileDB.prototype.dbExecutionSummaryByName = function( name ) {
	return this.fetchSingletonFromDB( "SELECT * FROM ExecutionSummaries WHERE NAME='" + name + "'" );
};

ProfileDB.prototype.dbChildTNodes = function( parentTNodeId ) {
	return this.fetchMultipleElemsFromDB( "SELECT * FROM TNodes WHERE parentId=" + parentTNodeId );
};

ProfileDB.prototype.threadCount = function() {
	var res = this.fetchSingletonFromDB( "SELECT * FROM AppData ");
	return res.THREAD_COUNT;
};

ProfileDB.prototype.ticTocRegionSummaries = function() {
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